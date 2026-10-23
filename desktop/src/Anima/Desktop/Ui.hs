{- | The window: the project list, and the editor (tools, canvas, timeline, playback, onion
skin) with the chat sidebar beside it. GTK runs on the main thread; anything slow (the
network, the disk) runs on its own thread and hands the result back with 'GLib.idleAdd'.

Edits go through the library: an item is made by its parser ("Anima.Desktop.Make"), a
change to the project by its edit functions, so the desktop cannot write what the server
would refuse. Every edit is undoable and saved a moment later, against the revision it
started from; if someone saved first, this version is kept as a copy.
-}
module Anima.Desktop.Ui (runApp, Window (..), Probe (..), Pointer (..), Phase (..), buildWindow, createProject) where

import Anima.Ids (newUuid)
import Anima.Tools (ToolCall (..), ToolSpec (..), instructions, int, projectTool, projectTools)
import Anima.Types.Parse (Parse (..))
import Anima.Types.Project hiding (duration)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forM, forM_, unless, void, when, zipWithM_, (>=>))
import Data.Bifunctor (bimap)
import Data.Char (toLower)
import Data.GI.Base (AttrOp (..), get, new, on)
import Data.GI.Base.ManagedPtr (castTo)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import GHC.Clock (getMonotonicTime)
import Numeric (showHex)
import System.FilePath (takeBaseName, takeExtension, (</>))

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEnc
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Connector qualified as Connector
import GI.Cairo.Render.Matrix qualified as M
import GI.GLib qualified as GLib
import GI.Gdk qualified as Gdk
import GI.GdkPixbuf qualified as Pixbuf
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango
import Network.HTTP.Client.TLS qualified as Tls

import Anima.Desktop.Assistant (Model (..), Provider (..), Reply (..), Settings (..), Stop (..), ToolOutcome (..), ToolUse (..), connectModel, defaultSettings, loadSettings, parseSettings, providerName, saveSettings)
import Anima.Desktop.Backend (Backend (..), Saved (..), localBackend, remoteBackend)
import Anima.Desktop.Config (Connection (..), cacheFolder, defaultFolder, loadConnection, saveConnection)
import Anima.Desktop.Draw (Pictures, drawCel, drawItem, drawItems)
import Anima.Desktop.Export (Export (..), Fit (..), Format (..), Quality (..), Size (..), Track (..), exportFilm, formatExtension, formatName, keepsAlpha, presets)
import Anima.Desktop.Geometry (Box (..), boxOf, topmostAt)
import Anima.Desktop.Make (Ink (..), ShapeTool (..))
import Anima.Desktop.Oidc (Oidc (..), Tokens (..), displayName, keychainAccount, newAuth, parseOidc, redirectPort, redirectUri, signIn)
import Anima.Desktop.Scene (Camera (..), Env (..), cameraAt, celAt, drawFrame, drawLayers, duration, newCache, placement, renderPng)
import Anima.Desktop.Secrets (ApiKey, forgetSecret, keyText, loadKey, loadSecret, parseApiKey, saveKey, saveSecret)
import Anima.Desktop.Style (accent, applyStyle, celEmpty, celFill, dimText, ground, hairline, iconImage, paperShadow, raised, rgb, rgba, rounded, surface, textColour, uiText, workspace)
import Anima.Desktop.Video (ffmpegPath, frameAt, lengthOf, newFrames)

import Anima.Desktop.Fill qualified as Fill
import Anima.Desktop.Geometry qualified as Geometry
import Anima.Desktop.Make qualified as Make

-- State ----------------------------------------------------------------------------------

data Open = Open
    { pid :: ProjectId
    , project :: Project
    , view :: ProjectView
    , atRevision :: Revision
    , env :: Env
    }

data Tool = SelectTool | BrushTool | ScatterTool | EraserTool | ShapeTool | FillTool | TextTool | CameraTool
    deriving stock (Eq, Show)

data SelMode = Moving | Scaling | Turning
    deriving stock (Eq)

-- | A selection being dragged: how, from where, about which point, and the change so far.
data SelDrag = SelDrag {mode :: SelMode, from :: (Double, Double), centre :: (Double, Double), current :: M.Matrix}

{- | The reference board: which reference is shown, how each is zoomed and panned, the
eyedropper, and the moment of each video that plays on its own.
-}
data Board = Board
    { showing :: Int
    , views :: Map.Map Text (Double, Double, Double)
    , picking :: Bool
    , own :: Map.Map Text Double
    , ownPlay :: Maybe GLib.Source
    , shown :: [(Text, Media, Bool)]
    , panFrom :: Maybe (Double, Double, Double, Double)
    }

-- | A camera key being edited: the key's settings under names of their own.
data Shot = Shot {atFrame :: Int, shotCentre :: (Double, Double), shotZoom :: Double, shotTurn :: Double, shotEase :: Ease, shotShake :: Double, shotFocus :: Double, shotBlur :: Double, shotTint :: Maybe Text, shotTintAmount :: Double}

shotOf :: CameraKeyView -> Shot
shotOf k = Shot k.frame k.center k.zoom k.rotation k.ease k.shake k.focus k.aperture k.tint k.tintAmount

{- | The assistant's conversation: the model, what was said (in its wire form), whether a
turn is running, and the lines on screen (who, what) for tests.
-}
data Chat = Chat {talker :: Maybe Model, history :: [Aeson.Value], busy :: Bool, cancelled :: Bool, shownLines :: [(Text, Text)]}

-- | A stroke or shape being drawn: its points in the layer's own space.
data Draft = Draft {pts :: [(Double, Double, Double)], start :: (Double, Double), seed :: Int}

data State = State
    { backend :: Maybe Backend
    , open :: Maybe Open
    , playhead :: Int
    , layer :: Int
    , playing :: Maybe GLib.Source
    , onion :: Bool
    , magnify :: Double
    , panX :: Double
    , panY :: Double
    , fitted :: Bool
    , drag :: Maybe (Double, Double, Double, Double)
    , tlScroll :: Double
    , tool :: Tool
    , brushKind :: Text
    , scatterKind :: Text
    , objectEraser :: Bool
    , shapeKind :: ShapeTool
    , filled :: Bool
    , colour :: Text
    , penSize :: Double
    , penAlpha :: Double
    , draft :: Maybe Draft
    , selection :: [Int]
    , selDrag :: Maybe SelDrag
    , camDrag :: Maybe ((Double, Double), (Double, Double))
    -- ^ The shot being dragged: where the pointer started and the shot's centre then.
    , past :: [Project]
    , future :: [Project]
    , dirty :: Bool
    , saveTimer :: Maybe GLib.Source
    , shiftHeld :: Bool
    , spaceHeld :: Bool
    , lastKey :: Maybe Text
    {- ^ The setting last changed through a control (a slider's drag), so its changes merge
    into one undo step.
    -}
    }

initial :: State
initial =
    State
        { backend = Nothing
        , open = Nothing
        , playhead = 0
        , layer = 0
        , playing = Nothing
        , onion = True
        , magnify = 1
        , panX = 0
        , panY = 0
        , fitted = False
        , drag = Nothing
        , tlScroll = 0
        , tool = BrushTool
        , brushKind = "pen"
        , scatterKind = "grass"
        , objectEraser = False
        , shapeKind = EllipseShape
        , filled = False
        , colour = "#1c1c1e"
        , penSize = 6
        , penAlpha = 1
        , draft = Nothing
        , selection = []
        , selDrag = Nothing
        , camDrag = Nothing
        , past = []
        , future = []
        , dirty = False
        , saveTimer = Nothing
        , shiftHeld = False
        , spaceHeld = False
        , lastKey = Nothing
        }

-- | Run on the GTK thread.
onMain :: IO () -> IO ()
onMain act = void (GLib.idleAdd GLib.PRIORITY_DEFAULT (act >> pure False))

background :: IO a -> (a -> IO ()) -> IO ()
background work done = void . forkIO $ do
    r <- work
    onMain (done r)

-- | Run on the GTK thread and wait for the answer (from another thread).
onMainWait :: IO a -> IO a
onMainWait act = do
    box <- newEmptyMVar
    onMain (try act >>= putMVar box)
    takeMVar box >>= either (\(e :: SomeException) -> throwIO e) pure

{- | The assistant's tools: the project's, and the editor's own (what is on screen, a
look at a frame, undo and redo).
-}
assistantTools :: [ToolSpec]
assistantTools =
    projectTools
        <> [ ToolSpec "editor_state" "What the person is looking at: the frame (from 1), the layer and drawing being edited, and how many items are selected." [] []
           , ToolSpec "preview" "See a frame as it exports: a PNG of the whole picture (every visible layer, through the camera). Use it to check what you drew." [("frame", int "Frame number on the timeline, from 1 (default 1)"), ("width", int "Image width in pixels, 64 to 1920 (default 960)")] []
           , ToolSpec "undo" "Undo the last change in the editor (yours or the person's)." [] []
           , ToolSpec "redo" "Redo the last undone change." [] []
           ]

-- | Once, after `ms` milliseconds.
after :: Int -> IO () -> IO GLib.Source
after ms act = do
    src <- GLib.timeoutSourceNew (fromIntegral ms)
    GLib.sourceSetCallback src (act >> pure False)
    _ <- GLib.sourceAttach src Nothing
    pure src

-- App -------------------------------------------------------------------------------------

-- | The window, and what a test (or 'runApp') needs to drive it.
data Window = Window
    { window :: Gtk.Window
    , canvasArea :: Gtk.DrawingArea
    , connectTo :: Connection -> IO ()
    , openId :: ProjectId -> IO ()
    , inspect :: IO Probe
    , say :: Text -> IO ()
    , pointer :: Pointer -> IO Bool
    , key :: Text -> Bool -> Bool -> IO Bool
    , keyUp :: Text -> IO Bool
    , click :: Text -> IO ()
    , addReferenceFile :: FilePath -> IO ()
    -- ^ A picture or video file onto the reference board, as + does after its dialog.
    , connectAssistant :: Maybe Model -> IO ()
    -- ^ The assistant's model (a test gives its own).
    , tellAssistant :: Text -> IO ()
    -- ^ A message to the assistant, as Send does.
    , exportFile :: Export -> FilePath -> IO ()
    -- ^ The film to a file, as Export does after its dialogs.
    , command :: Text -> IO ()
    -- ^ A menu command by its action name (undo, export, tool-brush…).
    , setShotWith :: Text -> Double -> IO ()
    -- ^ One of the camera bar's sliders set as a person would (zoom, turn, shake).
    , openLayerPanel :: Int -> IO ()
    , setInPanel :: Text -> Aeson.Value -> IO ()
    {- ^ One of the layer panel's controls set as a person would (name, opacity, depth,
    blend, clip, shadow, up, down).
    -}
    }

data Phase = Press | Move | Release
    deriving stock (Eq, Show)

{- | The pen, mouse or a finger on the canvas, in widget pixels: what the event handler
reads from GDK and what a test sends.
-}
data Pointer = Pointer {phase :: Phase, px :: Double, py :: Double, pressure :: Double, button :: Word, panning :: Bool, held :: Bool}

-- | What the window holds right now, as a test sees it.
data Probe = Probe
    { edited :: Maybe Project
    , toolName :: Text
    , chosen :: [Int]
    , undoSteps :: Int
    , unsaved :: Bool
    , note :: Text
    , onLayer :: Int
    , atFrame :: Int
    , screenOf :: (Double, Double) -> (Double, Double)
    , boardShows :: Bool
    -- ^ Whether the board has a picture or still on screen.
    , chatLog :: [(Text, Text)]
    -- ^ The assistant's conversation as shown: (user | assistant | step | failed | error, text).
    , chatBusy :: Bool
    }

runApp :: IO ()
runApp = do
    app <- new Gtk.Application [#applicationId := "io.github.abdivasiyev.anima2d"]
    _ <- on app #activate $ do
        -- The menu bar first, so macOS shows it with the window.
        menuBar app
        w <- buildWindow
        Gtk.applicationAddWindow app w.window
        commands app w
        -- First run asks where projects live; after that the choice is remembered.
        saved <- loadConnection
        c <- case saved of
            Just c -> pure (Just c)
            Nothing -> do
                r <- connectionDialog w.window Nothing
                forM_ r saveConnection
                pure r
        case c of
            Just conn -> w.connectTo conn
            Nothing -> w.say "Choose where projects live with Settings… (⌘,)"
    _ <- Gio.applicationRun app Nothing
    pure ()

{- | The menu bar, as a Mac app has it: the app's menu, File, Edit, View, Playback, Tools and
Help. GTK puts it in macOS's own menu bar; on Linux it is the window's.
-}
menuBar :: Gtk.Application -> IO ()
menuBar app = do
    let
        section items = do
            m <- Gio.menuNew
            forM_ items $ \(lbl, action) -> Gio.menuAppend m (Just lbl) (Just ("app." <> action))
            pure m
        menu sections = do
            m <- Gio.menuNew
            forM_ sections (section >=> Gio.menuAppendSection m Nothing)
            pure m
    appMenu <- menu [[("About anima2D", "about")], [("Settings…", "settings"), ("Connect a Model…", "connect-model")]]
    file <- menu [[("New Project…", "new-project"), ("Projects", "projects")], [("Export…", "export")]]
    edit <- menu [[("Undo", "undo"), ("Redo", "redo")], [("Select All", "select-all"), ("Delete", "delete")]]
    view <- menu [[("Fit to Window", "fit"), ("Onion Skin", "onion")], [("Show Sidebar", "sidebar"), ("Assistant", "assistant"), ("References", "references")]]
    playback <- menu [[("Play / Pause", "play")], [("Previous Frame", "prev-frame"), ("Next Frame", "next-frame")]]
    tools <- menu [[("Select", "tool-select"), ("Brush", "tool-brush"), ("Scatter", "tool-scatter"), ("Eraser", "tool-eraser"), ("Shapes", "tool-shapes"), ("Fill", "tool-fill"), ("Text", "tool-text"), ("Camera", "tool-camera")]]
    help <- menu [[("anima2D on GitHub", "help")]]
    bar <- Gio.menuNew
    forM_ [("File", file), ("Edit", edit), ("View", view), ("Playback", playback), ("Tools", tools), ("Help", help)] $ \(lbl, m) -> Gio.menuAppendSubmenu bar (Just lbl) m
    Gtk.applicationSetAppMenu app (Just appMenu)
    Gtk.applicationSetMenubar app (Just bar)
    forM_
        [ ("about", ["<Primary>i"])
        , ("settings", ["<Primary>comma"])
        , ("new-project", ["<Primary>n"])
        , ("projects", ["<Primary><Shift>p"])
        , ("export", ["<Primary>e"])
        , ("undo", ["<Primary>z"])
        , ("redo", ["<Primary><Shift>z"])
        , ("select-all", ["<Primary>a"])
        , ("fit", ["<Primary>0"])
        , ("sidebar", ["<Primary><Alt>s"])
        ]
        $ \(action, keys) -> Gtk.applicationSetAccelsForAction app ("app." <> action) keys

-- | Each menu item, an action on the app that the window carries out.
commands :: Gtk.Application -> Window -> IO ()
commands app w =
    forM_ ["about", "settings", "connect-model", "new-project", "projects", "export", "undo", "redo", "select-all", "delete", "fit", "onion", "sidebar", "assistant", "references", "play", "prev-frame", "next-frame", "help", "tool-select", "tool-brush", "tool-scatter", "tool-eraser", "tool-shapes", "tool-fill", "tool-text", "tool-camera"] $ \name -> do
        action <- Gio.simpleActionNew name Nothing
        _ <- on action #activate $ \_ -> case name of
            "about" -> aboutDialog w.window
            "help" -> Gtk.showUriOnWindow (Just w.window) "https://github.com/abdivasiyev/anima2d" 0
            _ -> w.command name
        Gio.actionMapAddAction app action

aboutDialog :: Gtk.Window -> IO ()
aboutDialog parent = do
    d <- Gtk.aboutDialogNew
    Gtk.windowSetTransientFor d (Just parent)
    Gtk.aboutDialogSetProgramName d "anima2D"
    Gtk.aboutDialogSetVersion d (Just "0.1.0")
    Gtk.aboutDialogSetComments d (Just "A 2D animation studio: draw, animate, light and film, with an assistant that draws with you.")
    Gtk.aboutDialogSetWebsite d (Just "https://github.com/abdivasiyev/anima2d")
    Gtk.aboutDialogSetLicenseType d Gtk.LicenseApache20
    _ <- Gtk.dialogRun d
    Gtk.widgetDestroy d

-- | Builds and shows the window; nothing is connected yet.
buildWindow :: IO Window
buildWindow = do
    applyStyle
    st <- newIORef initial
    lastCommand <- newIORef ("" :: Text, 0 :: Double)
    frames <- cacheFolder >>= newFrames
    videoFiles <- newIORef (Map.empty :: Map.Map Text (Maybe FilePath))
    boardRef <- newIORef Board {showing = 0, views = Map.empty, picking = False, own = Map.empty, ownPlay = Nothing, shown = [], panFrom = Nothing}
    boardSyncing <- newIORef False
    camSyncing <- newIORef False
    win <- new Gtk.Window [#title := "anima2D", #defaultWidth := 1360, #defaultHeight := 880]
    -- The title bar: back to the projects, undo and redo, whether it is saved; the
    -- connection, a new project (on the list) and the sidebar.
    header <- new Gtk.HeaderBar [#showCloseButton := True, #title := "anima2D"]
    Gtk.windowSetTitlebar win (Just header)
    backBtn <- iconButton "back" "Projects"
    undoBtn <- iconButton "undo" "Undo (⌘Z)"
    redoBtn <- iconButton "redo" "Redo (⇧⌘Z)"
    saveState <- Gtk.labelNew (Just "")
    addClass saveState "dim"
    connBtn <- iconButton "server" "Where projects live…"
    newBtn <- Gtk.buttonNewWithLabel "New project"
    addClass newBtn "primary"
    chatBtn <- Gtk.toggleButtonNew
    iconInto chatBtn "chat" "Sidebar"
    Gtk.toggleButtonSetActive chatBtn True
    Gtk.headerBarPackStart header backBtn
    Gtk.headerBarPackStart header undoBtn
    Gtk.headerBarPackStart header redoBtn
    Gtk.headerBarPackStart header saveState
    exportBtn <- Gtk.buttonNewWithLabel "Export"
    addClass exportBtn "primary"
    Gtk.headerBarPackEnd header chatBtn
    Gtk.headerBarPackEnd header exportBtn
    Gtk.headerBarPackEnd header connBtn
    Gtk.headerBarPackEnd header newBtn
    stack <- new Gtk.Stack [#transitionType := Gtk.StackTransitionTypeCrossfade, #transitionDuration := 120]
    -- Home: the projects, as cards with their first frame.
    flow <- new Gtk.FlowBox [#selectionMode := Gtk.SelectionModeNone, #maxChildrenPerLine := 8, #columnSpacing := 16, #rowSpacing := 16, #homogeneous := True, #valign := Gtk.AlignStart]
    homeTitle <- Gtk.labelNew (Just "Projects")
    addClass homeTitle "home-title"
    Gtk.labelSetXalign homeTitle 0
    status <- Gtk.labelNew (Just "")
    addClass status "dim"
    Gtk.labelSetXalign status 0
    Gtk.labelSetEllipsize status Pango.EllipsizeModeMiddle
    homeBody <- Gtk.boxNew Gtk.OrientationVertical 6
    forM_ [Gtk.widgetSetMarginStart, Gtk.widgetSetMarginEnd] $ \m -> m homeBody 32
    Gtk.widgetSetMarginTop homeBody 28
    Gtk.widgetSetMarginBottom homeBody 28
    Gtk.boxPackStart homeBody homeTitle False False 0
    Gtk.boxPackStart homeBody status False False 0
    Gtk.widgetSetMarginTop flow 18
    Gtk.boxPackStart homeBody flow False False 0
    home <- new Gtk.ScrolledWindow [#hscrollbarPolicy := Gtk.PolicyTypeNever]
    Gtk.containerAdd home homeBody
    -- Tools: a rail of icons; the bar above the canvas holds the chosen tool's options,
    -- then colour, size and opacity.
    selectR <- toolButton Nothing "select" "Select (V)"
    brushR <- toolButton (Just selectR) "pen" "Brush (B)"
    scatterR <- toolButton (Just selectR) "grass" "Scatter (G)"
    eraserR <- toolButton (Just selectR) "eraser" "Eraser (E)"
    shapeR <- toolButton (Just selectR) "shapes" "Shapes (U)"
    fillR <- toolButton (Just selectR) "fill" "Fill (K)"
    textR <- toolButton (Just selectR) "text" "Text (T)"
    cameraR <- toolButton (Just selectR) "camera" "Camera (C)"
    Gtk.toggleButtonSetActive brushR True
    rail <- Gtk.boxNew Gtk.OrientationVertical 2
    addClass rail "rail"
    pack rail [toW selectR, toW brushR, toW scatterR, toW eraserR, toW shapeR, toW fillR, toW textR, toW cameraR]
    toolName <- Gtk.labelNew (Just "Brush")
    addClass toolName "dim"
    Gtk.widgetSetSizeRequest toolName 64 (-1)
    Gtk.labelSetXalign toolName 0
    (brushKinds, pickBrush) <- segmented [(k, Right k, Text.toTitle k) | k <- ["pen", "pencil", "marker", "paint"]] (\v -> modifyIORef' st (\s -> s {brushKind = v}))
    (scatterKinds, _) <- segmented [(k, Right k, Text.toTitle k) | k <- ["grass", "birds", "stars", "leaves", "flowers", "spray"]] (\v -> modifyIORef' st (\s -> s {scatterKind = v}))
    (eraserKinds, _) <- segmented [("pixel", Left "Pixel", "Rub out what it passes over"), ("object", Left "Object", "Remove whole items")] (\v -> modifyIORef' st (\s -> s {objectEraser = v == "object"}))
    (shapeKinds, _) <-
        segmented
            [(k, Right k, Text.toTitle k) | k <- ["ellipse", "rect", "triangle", "line"]]
            ( \v ->
                modifyIORef' st $ \s ->
                    s
                        { shapeKind = case v of
                            "rect" -> RectShape
                            "triangle" -> TriangleShape
                            "line" -> LineShape
                            _ -> EllipseShape
                        }
            )
    pickBrush "pen"
    filledB <- Gtk.checkButtonNewWithLabel "Filled"
    colourB <- Gtk.colorButtonNew
    Gtk.widgetSetTooltipText colourB (Just "Colour")
    setColour colourB "#1c1c1e"
    sizeS <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal 0.5 200 0.5
    Gtk.rangeSetValue sizeS 6
    Gtk.widgetSetSizeRequest sizeS 130 (-1)
    alphaS <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal 0.05 1 0.05
    Gtk.rangeSetValue alphaS 1
    Gtk.widgetSetSizeRequest alphaS 90 (-1)
    forM_ [(sizeS, 0), (alphaS, 2)] $ \(sc, d) -> do
        Gtk.scaleSetDrawValue sc True
        Gtk.scaleSetValuePos sc Gtk.PositionTypeRight
        Gtk.scaleSetDigits sc d
    sizeL <- Gtk.labelNew (Just "Size")
    alphaL <- Gtk.labelNew (Just "Opacity")
    forM_ [sizeL, alphaL] (`addClass` "dim")
    -- The camera's bar: the shot at this frame, each change its key here.
    camKeyL <- Gtk.labelNew (Just "")
    addClass camKeyL "dim"
    camZoom <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal 0.1 10 0.01
    camTurn <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal (-180) 180 1
    camShake <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal 0 60 1
    camAperture <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal 0 20 0.5
    camFocus <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal 0.2 20 0.1
    camTintB <- Gtk.checkButtonNewWithLabel "Tint"
    camTintC <- Gtk.colorButtonNew
    camTintA <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal 0 1 0.01
    camEase <- Gtk.comboBoxTextNew
    forM_ [("inOut", "Ease in and out"), ("in", "Ease in"), ("out", "Ease out"), ("linear", "Linear")] $ \(i, n) -> Gtk.comboBoxTextAppend camEase (Just i) n
    camRemove <- iconButton "remove" "Remove the key at this frame"
    forM_ [(camZoom, 2, 90), (camTurn, 0, 80), (camShake, 0, 70), (camAperture, 1, 70), (camFocus, 1, 70), (camTintA, 2, 60)] $ \(sc, d, w) -> do
        Gtk.scaleSetValuePos sc Gtk.PositionTypeRight
        Gtk.scaleSetDigits sc d
        Gtk.widgetSetSizeRequest sc w (-1)
    camBar <- Gtk.boxNew Gtk.OrientationHorizontal 6
    camLabels <- forM ["Zoom", "Turn", "Shake", "Blur", "Focus"] $ \t -> Gtk.labelNew (Just t) >>= \l -> addClass l "dim" >> pure l
    case camLabels of
        [lz, lt, ls, lb, lf] -> pack camBar [toW camKeyL, toW lz, toW camZoom, toW lt, toW camTurn, toW ls, toW camShake, toW lb, toW camAperture, toW lf, toW camFocus, toW camTintB, toW camTintC, toW camTintA, toW camEase, toW camRemove]
        _ -> pure ()
    tools <- Gtk.boxNew Gtk.OrientationHorizontal 8
    addClass tools "bar"
    pack tools [toW toolName, toW brushKinds, toW scatterKinds, toW eraserKinds, toW shapeKinds, toW filledB, toW camBar]
    toolsEnd <- sequence [toW colourB, separator, toW sizeL, toW sizeS, toW alphaL, toW alphaS]
    forM_ (reverse toolsEnd) $ \w -> Gtk.boxPackEnd tools w False False 0
    options <- sequence [toW brushKinds, toW scatterKinds, toW eraserKinds, toW shapeKinds, toW filledB, toW camBar]
    let
        showOptions t = do
            Gtk.labelSetText toolName $ case t of
                SelectTool -> "Select"
                BrushTool -> "Brush"
                ScatterTool -> "Scatter"
                EraserTool -> "Eraser"
                ShapeTool -> "Shapes"
                FillTool -> "Fill"
                TextTool -> "Text"
                CameraTool -> "Camera"
            zipWithM_ Gtk.widgetSetVisible options $ case t of
                BrushTool -> [True, False, False, False, False, False]
                ScatterTool -> [False, True, False, False, False, False]
                EraserTool -> [False, False, True, False, False, False]
                ShapeTool -> [False, False, False, True, True, False]
                CameraTool -> [False, False, False, False, False, True]
                _ -> [False, False, False, False, False, False]
            -- The pen's colour and size mean nothing to the camera.
            mapM_ (`Gtk.widgetSetVisible` (t /= CameraTool)) toolsEnd
    -- Editor: the bar over the rail and canvas, over the transport and the timeline; the
    -- sidebar on the right.
    canvas <- new Gtk.DrawingArea [#canFocus := True]
    timeline <- new Gtk.DrawingArea []
    Gtk.widgetSetSizeRequest timeline (-1) 168
    forM_ [canvas, timeline] $ \a -> Gtk.widgetAddEvents a [Gdk.EventMaskButtonPressMask, Gdk.EventMaskButtonReleaseMask, Gdk.EventMaskPointerMotionMask, Gdk.EventMaskScrollMask, Gdk.EventMaskSmoothScrollMask]
    prevBtn <- iconButton "prev" "Previous frame (←)"
    playBtn <- iconButton "play" "Play (Space)"
    nextBtn <- iconButton "next" "Next frame (→)"
    counter <- Gtk.labelNew (Just "")
    addClass counter "counter"
    newCelB <- iconButton "newDrawing" "New drawing (N)"
    dupCelB <- iconButton "duplicate" "Duplicate drawing (D)"
    delCelB <- iconButton "remove" "Delete drawing"
    lessB <- iconButton "minus" "Hold one frame less"
    holdL <- Gtk.labelNew (Just "Hold")
    addClass holdL "dim"
    moreB <- iconButton "add" "Hold one frame more"
    addLayerB <- iconButton "newLayer" "New layer"
    hideB <- iconButton "eye" "Hide layer"
    lockB <- iconButton "unlock" "Lock layer"
    delLayerB <- iconButton "remove" "Delete layer"
    onionBtn <- Gtk.toggleButtonNew
    iconInto onionBtn "onion" "Onion skin (O)"
    Gtk.toggleButtonSetActive onionBtn True
    fitBtn <- iconButton "fit" "Fit to window (0)"
    transport <- Gtk.boxNew Gtk.OrientationHorizontal 2
    addClass transport "transport"
    pack transport [toW prevBtn, toW playBtn, toW nextBtn, toW counter, separator, toW newCelB, toW dupCelB, toW delCelB, separator, toW lessB, toW holdL, toW moreB, separator, toW addLayerB, toW hideB, toW lockB, toW delLayerB]
    Gtk.boxPackEnd transport fitBtn False False 0
    Gtk.boxPackEnd transport onionBtn False False 0
    left <- Gtk.boxNew Gtk.OrientationVertical 0
    work <- Gtk.boxNew Gtk.OrientationHorizontal 0
    Gtk.boxPackStart work rail False False 0
    Gtk.boxPackStart work canvas True True 0
    Gtk.boxPackStart left tools False False 0
    Gtk.boxPackStart left work True True 0
    Gtk.boxPackStart left transport False False 0
    Gtk.boxPackStart left timeline False False 0
    -- The sidebar: the reference board and the assistant, one at a time.
    boardChips <- Gtk.boxNew Gtk.OrientationHorizontal 4
    chipsScroll <- new Gtk.ScrolledWindow [#vscrollbarPolicy := Gtk.PolicyTypeNever, #hscrollbarPolicy := Gtk.PolicyTypeAutomatic]
    Gtk.containerAdd chipsScroll boardChips
    boardAdd <- iconButton "add" "Add a picture or video…"
    boardTop <- Gtk.boxNew Gtk.OrientationHorizontal 4
    addClass boardTop "board-top"
    Gtk.boxPackStart boardTop chipsScroll True True 0
    Gtk.boxPackEnd boardTop boardAdd False False 0
    boardView <- new Gtk.DrawingArea []
    Gtk.widgetAddEvents boardView [Gdk.EventMaskButtonPressMask, Gdk.EventMaskButtonReleaseMask, Gdk.EventMaskPointerMotionMask, Gdk.EventMaskScrollMask, Gdk.EventMaskSmoothScrollMask]
    boardEmpty <- Gtk.labelNew (Just "Add a picture or video to keep in view while you draw: + above.")
    addClass boardEmpty "dim"
    Gtk.labelSetLineWrap boardEmpty True
    Gtk.labelSetMaxWidthChars boardEmpty 30
    Gtk.labelSetJustify boardEmpty Gtk.JustificationCenter
    followB <- Gtk.toggleButtonNew
    iconInto followB "pin" "Follow the playhead"
    ownPlayB <- iconButton "play" "Play the video"
    scrubS <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal 0 1 0.001
    Gtk.scaleSetDrawValue scrubS False
    pickB <- Gtk.toggleButtonNew
    iconInto pickB "eyedropper" "Take a colour from it"
    traceB <- iconButton "trace" "Trace it: put it under the drawings"
    removeRefB <- iconButton "remove" "Remove from the board"
    videoRow <- Gtk.boxNew Gtk.OrientationHorizontal 2
    Gtk.boxPackStart videoRow followB False False 0
    Gtk.boxPackStart videoRow ownPlayB False False 0
    Gtk.boxPackStart videoRow scrubS True True 0
    boardFoot <- Gtk.boxNew Gtk.OrientationHorizontal 2
    addClass boardFoot "transport"
    Gtk.boxPackStart boardFoot videoRow True True 0
    forM_ [toW removeRefB, toW traceB, toW pickB] $ \mk -> mk >>= \x -> Gtk.boxPackEnd boardFoot x False False 0
    boardPage <- Gtk.boxNew Gtk.OrientationVertical 0
    Gtk.boxPackStart boardPage boardTop False False 0
    Gtk.boxPackStart boardPage boardView True True 0
    Gtk.boxPackStart boardPage boardEmpty True True 0
    Gtk.boxPackStart boardPage boardFoot False False 0
    -- The assistant: a conversation with a model that edits the open project.
    chatRef <- newIORef Chat {talker = Nothing, history = [], busy = False, cancelled = False, shownLines = []}
    chatModelL <- Gtk.labelNew (Just "Not connected")
    addClass chatModelL "dim"
    Gtk.labelSetXalign chatModelL 0
    Gtk.labelSetEllipsize chatModelL Pango.EllipsizeModeEnd
    chatNewB <- iconButton "newChat" "New conversation"
    chatSetB <- iconButton "settings" "Connect a model…"
    chatTop <- Gtk.boxNew Gtk.OrientationHorizontal 2
    addClass chatTop "board-top"
    Gtk.widgetSetMarginStart chatModelL 6
    Gtk.boxPackStart chatTop chatModelL True True 0
    Gtk.boxPackEnd chatTop chatSetB False False 0
    Gtk.boxPackEnd chatTop chatNewB False False 0
    chatList <- Gtk.boxNew Gtk.OrientationVertical 10
    forM_ [Gtk.widgetSetMarginStart, Gtk.widgetSetMarginEnd, Gtk.widgetSetMarginTop, Gtk.widgetSetMarginBottom] $ \m -> m chatList 12
    chatScroll <- new Gtk.ScrolledWindow [#hscrollbarPolicy := Gtk.PolicyTypeNever]
    Gtk.containerAdd chatScroll chatList
    chatEmpty <- Gtk.labelNew (Just "Ask for a drawing, a change or an animation. The assistant works on this project with the editor's own tools, and every change it makes can be undone with ⌘Z.")
    addClass chatEmpty "dim"
    Gtk.labelSetLineWrap chatEmpty True
    Gtk.labelSetMaxWidthChars chatEmpty 32
    Gtk.labelSetJustify chatEmpty Gtk.JustificationCenter
    Gtk.boxPackStart chatList chatEmpty False False 24
    composer <- new Gtk.TextView [#wrapMode := Gtk.WrapModeWordChar, #acceptsTab := False, #leftMargin := 2, #rightMargin := 2]
    addClass composer "composer"
    composerScroll <- new Gtk.ScrolledWindow [#hscrollbarPolicy := Gtk.PolicyTypeNever]
    Gtk.widgetSetSizeRequest composerScroll (-1) 64
    Gtk.containerAdd composerScroll composer
    addClass composerScroll "composer-box"
    sendB <- iconButton "send" "Send (Enter)"
    addClass sendB "primary"
    stopB <- iconButton "stop" "Stop"
    chatHint <- Gtk.labelNew (Just "Enter sends · Shift+Enter starts a new line")
    addClass chatHint "dim"
    Gtk.labelSetXalign chatHint 0
    composerRow <- Gtk.boxNew Gtk.OrientationHorizontal 6
    Gtk.boxPackStart composerRow composerScroll True True 0
    sendCol <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.boxPackEnd sendCol sendB False False 0
    Gtk.boxPackEnd sendCol stopB False False 0
    Gtk.boxPackStart composerRow sendCol False False 0
    chatBottom <- Gtk.boxNew Gtk.OrientationVertical 4
    addClass chatBottom "composer-bar"
    Gtk.boxPackStart chatBottom composerRow False False 0
    Gtk.boxPackStart chatBottom chatHint False False 0
    assistant <- Gtk.boxNew Gtk.OrientationVertical 0
    Gtk.boxPackStart assistant chatTop False False 0
    Gtk.boxPackStart assistant chatScroll True True 0
    Gtk.boxPackStart assistant chatBottom False False 0
    side <- new Gtk.Stack [#transitionType := Gtk.StackTransitionTypeNone]
    Gtk.stackAddTitled side assistant "assistant" "Assistant"
    Gtk.stackAddTitled side boardPage "board" "References"
    switcher <- Gtk.stackSwitcherNew
    Gtk.stackSwitcherSetStack switcher (Just side)
    Gtk.widgetSetHalign switcher Gtk.AlignCenter
    Gtk.widgetSetMarginTop switcher 8
    Gtk.widgetSetMarginBottom switcher 6
    chat <- Gtk.boxNew Gtk.OrientationVertical 0
    addClass chat "sidebar"
    Gtk.widgetSetSizeRequest chat 320 (-1)
    Gtk.boxPackStart chat switcher False False 0
    Gtk.boxPackStart chat side True True 0
    paned <- Gtk.panedNew Gtk.OrientationHorizontal
    Gtk.panedPack1 paned left True False
    Gtk.panedPack2 paned chat False False
    Gtk.panedSetPosition paned 1040
    _ <- on chatBtn #toggled (Gtk.toggleButtonGetActive chatBtn >>= Gtk.widgetSetVisible chat)
    Gtk.stackAddNamed stack home "home"
    Gtk.stackAddNamed stack paned "editor"
    Gtk.containerAdd win stack
    layerIcons <- newIORef Nothing
    let
        -- What the title bar offers on the list, and in the editor.
        chrome editing = do
            forM_ [backBtn, undoBtn, redoBtn] $ \x -> Gtk.widgetSetVisible x editing
            Gtk.widgetSetVisible saveState editing
            Gtk.widgetSetVisible chatBtn editing
            Gtk.widgetSetVisible exportBtn editing
            Gtk.widgetSetVisible newBtn (not editing)
    let
        redraw = Gtk.widgetQueueDraw canvas >> Gtk.widgetQueueDraw timeline >> showCounter >> refreshBoard
        -- Videos: the local file (fetched in the background for a server), then a still.
        videoFile asset = do
            known <- Map.lookup asset <$> readIORef videoFiles
            case known of
                Just path -> pure path
                Nothing -> do
                    modifyIORef' videoFiles (Map.insert asset Nothing)
                    s <- readIORef st
                    forM_ s.backend $ \b -> background (b.picture asset) $ \path -> modifyIORef' videoFiles (Map.insert asset path) >> redraw
                    pure Nothing
        videoStill asset seconds = videoFile asset >>= maybe (pure Nothing) (\path -> frameAt frames asset path seconds (onMain (Gtk.widgetQueueDraw canvas >> Gtk.widgetQueueDraw boardView)))
        boardRefs = do
            s <- readIORef st
            pure (maybe [] (\o -> o.view.board) s.open)
        currentRef = do
            rs <- boardRefs
            b <- readIORef boardRef
            pure (lookup b.showing (zip [0 ..] rs))
        -- The board's chips and buttons, from the project's references.
        refreshBoard = do
            rs <- boardRefs
            b <- readIORef boardRef
            let
                now = [(r.name, r.media, r.follows) | r <- rs]
                chosen' = max 0 (min b.showing (length rs - 1))
            when (now /= b.shown || chosen' /= b.showing) $ do
                writeIORef boardRef b {shown = now, showing = chosen'}
                kids <- Gtk.containerGetChildren boardChips
                mapM_ Gtk.widgetDestroy kids
                forM_ (zip [0 ..] rs) $ \(i, r) -> do
                    chip <- Gtk.buttonNewWithLabel r.name
                    addClass chip "chip"
                    when (i == chosen') (addClass chip "on")
                    _ <- on chip #clicked $ do
                        modifyIORef' boardRef (\x -> x {showing = i, shown = [], picking = False})
                        refreshBoard
                    Gtk.boxPackStart boardChips chip False False 0
                Gtk.widgetShowAll boardChips
            cur <- currentRef
            bd <- readIORef boardRef
            Gtk.widgetSetVisible chipsScroll (length rs > 1)
            Gtk.widgetSetVisible boardView (isJust cur)
            Gtk.widgetSetVisible boardEmpty (isNothing cur)
            Gtk.widgetSetVisible boardFoot (isJust cur)
            writeIORef boardSyncing True
            case cur of
                Just r | r.media == Video -> do
                    Gtk.widgetSetVisible videoRow True
                    Gtk.toggleButtonSetActive followB r.follows
                    Gtk.widgetSetVisible ownPlayB (not r.follows)
                    Gtk.widgetSetVisible scrubS (not r.follows)
                _ -> Gtk.widgetSetVisible videoRow False
            Gtk.toggleButtonSetActive pickB bd.picking
            writeIORef boardSyncing False
            Gtk.widgetQueueDraw boardView
        stopOwnPlay = do
            b <- readIORef boardRef
            forM_ b.ownPlay GLib.sourceDestroy
            writeIORef boardRef b {ownPlay = Nothing}
            iconInto ownPlayB "play" "Play the video"
        -- Every picture again, after one is added (the canvas and the board draw from them).
        refreshPictures = do
            s <- readIORef st
            case (s.backend, s.open) of
                (Just b, Just o) -> background (loadPictures b o.view) $ \pics -> do
                    modifyIORef' st (\x -> x {open = fmap (\o2 -> o2 {env = o2.env {pictures = pics}}) x.open})
                    redraw
                _ -> pure ()
        showCounter = do
            s <- readIORef st
            case s.open of
                Nothing -> Gtk.labelSetText counter ""
                Just o -> Gtk.labelSetText counter (Text.pack (show (s.playhead + 1) <> " / " <> show (duration o.view)))
            Gtk.widgetSetSensitive undoBtn (not (null s.past))
            Gtk.widgetSetSensitive redoBtn (not (null s.future))
            -- The layer buttons say what pressing them does to the edited layer.
            forM_ (s.open >>= \o -> lookup s.layer (zip [0 ..] o.view.stack)) $ \l -> do
                was <- readIORef layerIcons
                unless (was == Just (l.visible, l.locked)) $ do
                    writeIORef layerIcons (Just (l.visible, l.locked))
                    iconInto hideB (if l.visible then "eye" else "eyeOff") (if l.visible then "Hide layer" else "Show layer")
                    iconInto lockB (if l.locked then "lock" else "unlock") (if l.locked then "Unlock layer" else "Lock layer")
        say t = Gtk.labelSetText status t >> Gtk.labelSetText saveState t
        stop = do
            s <- readIORef st
            forM_ s.playing GLib.sourceDestroy
            writeIORef st s {playing = Nothing}
            iconInto playBtn "play" "Play (Space)"
        goHome = do
            stop
            saveNow
            modifyIORef' st (\s -> s {open = Nothing, past = [], future = []})
            Gtk.stackSetVisibleChildName stack "home"
            chrome False
            Gtk.headerBarSetSubtitle header Nothing
            refreshList
        refreshList = do
            s <- readIORef st
            forM_ s.backend $ \b -> do
                Gtk.labelSetText status ("Loading projects from " <> b.describe <> "…")
                background b.list $ \case
                    Left why -> Gtk.labelSetText status ("The projects could not be listed: " <> why)
                    Right ps -> do
                        kids <- Gtk.containerGetChildren flow
                        mapM_ Gtk.widgetDestroy kids
                        Gtk.labelSetText status (if null ps then "No projects yet: start one with New project." else b.describe)
                        forM_ ps $ \p -> projectCard b p openProject >>= Gtk.containerAdd flow
                        Gtk.widgetShowAll flow
        openProject pid = do
            s <- readIORef st
            forM_ s.backend $ \b -> do
                Gtk.labelSetText status "Opening…"
                background (b.load pid) $ \case
                    Left why -> Gtk.labelSetText status ("It could not be opened: " <> why)
                    Right (p, rev) -> do
                        o <- opened b pid p rev Nothing
                        modifyIORef' st (\x -> x {open = Just o, playhead = 0, layer = topDrawing o.view, fitted = False, past = [], future = [], dirty = False})
                        Gtk.headerBarSetSubtitle header (Just o.view.title)
                        Gtk.stackSetVisibleChildName stack "editor"
                        chrome True
                        Gtk.labelSetText saveState "Saved"
                        redraw
        connect c = do
            stop
            cacheDir <- cacheFolder
            b <- case c of
                Local folder -> localBackend folder
                Remote url hs sso -> do
                    manager <- Tls.newTlsManager
                    auth <- traverse (\o -> newAuth manager o Nothing) sso
                    remoteBackend manager url hs auth cacheDir
            modifyIORef' st (\s -> s {backend = Just b, open = Nothing})
            goHome
            watch b
        -- Live updates: wait on the backend for saves; reload the open project if it changed
        -- and nothing here is waiting to be saved.
        watch b = void . forkIO $ do
            let
                loop seen = do
                    n <- b.changes seen
                    s <- readIORef st
                    case (n, fmap (.describe) s.backend) of
                        (_, Just d) | d /= b.describe -> pure ()
                        (Nothing, _) -> GLib.usleep 30000000 >> loop Nothing
                        (Just k, _) -> do
                            when (isJust seen && n /= seen) (onMain reloadOpen)
                            loop (Just k)
            loop Nothing
        reloadOpen = do
            s <- readIORef st
            case (s.backend, s.open) of
                (Just b, Just o) | not s.dirty && isNothing s.draft -> background (b.load o.pid) $ \case
                    Right (p, rev) | rev /= o.atRevision -> do
                        s' <- readIORef st
                        unless s'.dirty $ do
                            o' <- opened b o.pid p rev (Just o.env.pictures)
                            writeIORef st s' {open = Just o'}
                            redraw
                    _ -> pure ()
                (Just _, Nothing) -> refreshList
                _ -> pure ()
        -- Editing: every change is a new project from the library's edits.
        commit f = do
            s <- readIORef st
            forM_ s.open $ \o -> case f o.project of
                Left why -> say why
                Right p' -> do
                    o' <- reopen o p'
                    writeIORef st s {open = Just o', past = take 200 (o.project : s.past), future = [], dirty = True, lastKey = Nothing}
                    Gtk.labelSetText saveState "Saving…"
                    scheduleSave
                    redraw
        undo = do
            s <- readIORef st
            case (s.open, s.past) of
                (Just o, p : rest) -> do
                    o' <- reopen o p
                    writeIORef st s {open = Just o', past = rest, future = o.project : s.future, dirty = True, lastKey = Nothing}
                    scheduleSave >> redraw
                _ -> pure ()
        redo = do
            s <- readIORef st
            case (s.open, s.future) of
                (Just o, p : rest) -> do
                    o' <- reopen o p
                    writeIORef st s {open = Just o', future = rest, past = o.project : s.past, dirty = True, lastKey = Nothing}
                    scheduleSave >> redraw
                _ -> pure ()
        scheduleSave = do
            s <- readIORef st
            forM_ s.saveTimer GLib.sourceDestroy
            t <- after 1200 saveNow
            writeIORef st s {saveTimer = Just t}
        saveNow = do
            s <- readIORef st
            case (s.backend, s.open) of
                (Just b, Just o) | s.dirty -> do
                    let
                        sent = o.project
                    background (b.save (Just o.atRevision) sent) $ \case
                        Saved rev -> do
                            modifyIORef' st $ \x -> case x.open of
                                Just o2 -> x {open = Just o2 {atRevision = rev}, dirty = x.dirty && o2.project /= sent}
                                Nothing -> x
                            x <- readIORef st
                            Gtk.labelSetText saveState (if x.dirty then "Saving…" else "Saved")
                        Stale _ -> do
                            -- Someone saved first: keep this version as a copy, then show theirs.
                            copied <- copyOf sent
                            forM_ copied $ \c -> void (forkIO (void (b.save Nothing c)))
                            modifyIORef' st (\x -> x {dirty = False})
                            say "Saved elsewhere meanwhile: this version is kept as a copy"
                            reloadOpen
                        Failed why -> do
                            Gtk.labelSetText saveState ("Not saved: " <> Text.take 60 why <> " (trying again)")
                            void (after 5000 saveNow)
                _ -> pure ()
        -- Where the pen is: on the edited layer's drawing at the playhead.
        target = do
            s <- readIORef st
            pure $ case s.open of
                Nothing -> Left "Open a project first"
                Just o -> case lookup s.layer (zip [0 ..] o.view.stack) of
                    Just l
                        | DrawingView holdLast cels <- l.kind ->
                            if l.locked
                                then Left "This layer is locked"
                                else maybe (Left "This layer ends before here: add a drawing first") (\ci -> Right (o, l, ci + 1)) (celAt holdLast cels s.playhead)
                    _ -> Left "Choose a drawing layer to draw on"
        toLayer s (x, y) = case s.open >>= \o -> lookup s.layer (zip [0 ..] o.view.stack) of
            Nothing -> canvasPoint s (x, y)
            Just l ->
                let
                    (m, _) = placement l s.playhead
                in
                    M.transformPoint (M.invert m) (canvasPoint s (x, y))
        pen s = Ink {color = s.colour, size = s.penSize, opacity = s.penAlpha}
        finish s d = case s.tool of
            BrushTool -> Just (Make.stroke s.brushKind (pen s) d.pts)
            ScatterTool -> Just (Make.scatterPath s.scatterKind (pen s) d.seed d.pts)
            EraserTool | not s.objectEraser -> Just (Make.erase s.penSize d.pts)
            ShapeTool -> case d.pts of
                [] -> Nothing
                _ -> let (x, y, _) = last d.pts in Just (Make.shape s.shapeKind (pen s) s.filled s.shiftHeld d.start (x, y))
            _ -> Nothing
        step d = do
            s <- readIORef st
            forM_ s.open $ \o -> do
                writeIORef st s {playhead = (s.playhead + d) `mod` max 1 (duration o.view)}
                redraw
                when (s.tool == CameraTool) refreshCamera
        play = do
            s <- readIORef st
            case (s.playing, s.open) of
                (Just _, _) -> stop
                (Nothing, Just o) -> do
                    src <- GLib.timeoutSourceNew (fromIntegral (max 1 (1000 `div` o.view.rate)))
                    GLib.sourceSetCallback src (step 1 >> pure True)
                    _ <- GLib.sourceAttach src Nothing
                    writeIORef st s {playing = Just src}
                    iconInto playBtn "pause" "Pause (Space)"
                (Nothing, Nothing) -> pure ()
        celEdit f = do
            t <- target
            case t of
                Left why -> say why
                Right (_, _, n) -> do
                    s <- readIORef st
                    commit (f s.layer n)
        layerEdit f = do
            s <- readIORef st
            commit (f s.layer)
        restyle key val = layerEdit (\li p -> restyleLayer li (KeyMap.fromList [(key, val)]) p)
        -- A change through a control: the same setting again (a slider dragged) replaces the
        -- last change instead of adding an undo step.
        commitKeyed k f = do
            s <- readIORef st
            case s.open of
                Just o
                    | s.lastKey == Just k
                    , not (null s.past) -> case f o.project of
                        Left why -> say why
                        Right p' -> do
                            o' <- reopen o p'
                            writeIORef st s {open = Just o', future = [], dirty = True}
                            Gtk.labelSetText saveState "Saving…"
                            scheduleSave
                            redraw
                _ -> commit f >> modifyIORef' st (\x -> x {lastKey = Just k})
        -- The camera at the playhead, as a key: the one there, or where the camera is between
        -- keys, or the whole canvas when there is no camera yet.
        shotHere = do
            s <- readIORef st
            pure $ case s.open of
                Nothing -> Nothing
                Just o ->
                    let
                        v = o.view
                        keyed = [shotOf k | k <- v.shots, k.frame == s.playhead]
                        made = case cameraAt v s.playhead of
                            Just c -> Shot s.playhead (c.cx, c.cy) c.zoom c.rotation EaseInOut c.shake c.focus c.aperture (fmap rgbHex c.tint) c.tintAmount
                            Nothing -> Shot s.playhead (fromIntegral v.width / 2, fromIntegral v.height / 2) 1 0 EaseInOut 0 1 0 Nothing 0
                    in
                        Just (case keyed of k : _ -> (True, k); [] -> (False, made))
        -- The key at the playhead changed by `f` (added if there was none), as one edit.
        setShot what f = do
            s <- readIORef st
            here <- shotHere
            syncing <- readIORef camSyncing
            forM_ ((,) <$> s.open <*> here) $ \(o, (_, k)) -> unless syncing $ do
                let
                    k' = f k
                    keys = [shotOf x | x <- o.view.shots, x.frame /= s.playhead] <> [k']
                    ordered = foldr insertByFrame [] keys
                    insertByFrame x [] = [x]
                    insertByFrame x (y : ys) = if x.atFrame <= y.atFrame then x : y : ys else y : insertByFrame x ys
                commitKeyed ("camera-" <> what <> Text.pack (show s.playhead)) (setCamera (Aeson.toJSON (map shotValue ordered)))
                refreshCamera
        refreshCamera = do
            here <- shotHere
            forM_ here $ \(keyed, k) -> do
                writeIORef camSyncing True
                Gtk.labelSetText camKeyL (if keyed then "Key at this frame" else "No key here: a change adds one")
                Gtk.rangeSetValue camZoom k.shotZoom
                Gtk.rangeSetValue camTurn (k.shotTurn * 180 / pi)
                Gtk.rangeSetValue camShake k.shotShake
                Gtk.rangeSetValue camAperture k.shotBlur
                Gtk.rangeSetValue camFocus k.shotFocus
                Gtk.toggleButtonSetActive camTintB (isJust k.shotTint)
                setColour camTintC (fromMaybe "#ff9a5c" k.shotTint)
                Gtk.rangeSetValue camTintA k.shotTintAmount
                _ <- Gtk.comboBoxSetActiveId camEase (Just (easeId k.shotEase))
                Gtk.widgetSetSensitive camRemove keyed
                writeIORef camSyncing False
        -- The object eraser: the topmost item under the pen on the edited drawing goes.
        eraseAt (lx, ly) s = case s.open >>= \o -> lookup s.layer (zip [0 ..] o.view.stack) of
            Just LayerView {kind = DrawingView holdLast cels}
                | Just ci <- celAt holdLast cels s.playhead
                , Just cel <- lookup ci (zip [0 ..] cels)
                , hit : _ <- [i | (i, item) <- reverse (zip [0 :: Int ..] cel.items), Geometry.hits item lx ly (max 6 (s.penSize / 2))] ->
                    commit (removeItemAt s.layer (ci + 1) hit)
            _ -> pure ()
    -- Header and transport.
    _ <- on backBtn #clicked goHome
    _ <- on undoBtn #clicked undo
    _ <- on redoBtn #clicked redo
    _ <- on prevBtn #clicked (stop >> step (-1))
    _ <- on nextBtn #clicked (stop >> step 1)
    _ <- on playBtn #clicked play
    _ <- on onionBtn #toggled $ do
        on' <- Gtk.toggleButtonGetActive onionBtn
        modifyIORef' st (\s -> s {onion = on'})
        redraw
    _ <- on fitBtn #clicked (modifyIORef' st (\s -> s {fitted = False}) >> redraw)
    _ <- on newCelB #clicked (celEdit (\li n -> insertDrawing li n False 1))
    _ <- on dupCelB #clicked (celEdit (\li n -> insertDrawing li n True 1))
    _ <- on delCelB #clicked (celEdit removeDrawing)
    _ <- on lessB #clicked (celEdit (\li n p -> holdOf li n p >>= \h -> setHoldAt li n (max 1 (h - 1)) p))
    _ <- on moreB #clicked (celEdit (\li n p -> holdOf li n p >>= \h -> setHoldAt li n (min 24 (h + 1)) p))
    _ <- on addLayerB #clicked $ do
        s <- readIORef st
        let
            n = maybe 1 (\o -> length o.view.stack + 1) s.open
        commit (addLayer ("Layer " <> Text.pack (show n)) False)
        modifyIORef' st (\x -> x {layer = maybe 0 (\o -> length o.view.stack - 1) x.open})
    _ <- on hideB #clicked $ do
        s <- readIORef st
        forM_ (s.open >>= \o -> lookup s.layer (zip [0 ..] o.view.stack)) $ \l -> restyle "visible" (Aeson.Bool (not l.visible))
    _ <- on lockB #clicked $ do
        s <- readIORef st
        forM_ (s.open >>= \o -> lookup s.layer (zip [0 ..] o.view.stack)) $ \l -> restyle "locked" (Aeson.Bool (not l.locked))
    _ <- on delLayerB #clicked $ do
        layerEdit removeLayerAt
        modifyIORef' st (\x -> x {layer = max 0 (x.layer - 1)})
        redraw
    _ <- on connBtn #clicked $ do
        current <- loadConnection
        chosen <- connectionDialog win current
        forM_ chosen $ \c -> saveConnection c >> connect c
    _ <- on newBtn #clicked $ do
        s <- readIORef st
        forM_ s.backend $ \b -> do
            name <- askText win "New project" "Untitled"
            forM_ name $ \nm -> background (createProject b nm) $ \case
                Left why -> Gtk.labelSetText status ("It could not be made: " <> why)
                Right pid -> openProject pid
    -- Tools.
    let
        pick t = do
            modifyIORef' st (\s -> s {tool = t, draft = Nothing, selection = if t == SelectTool then s.selection else [], selDrag = Nothing})
            showOptions t
            Gtk.widgetQueueDraw canvas
    _ <- on selectR #toggled (pick SelectTool)
    _ <- on fillR #toggled (pick FillTool)
    _ <- on brushR #toggled (pick BrushTool)
    _ <- on scatterR #toggled (pick ScatterTool)
    _ <- on eraserR #toggled (pick EraserTool)
    _ <- on shapeR #toggled (pick ShapeTool)
    _ <- on textR #toggled (pick TextTool)
    _ <- on cameraR #toggled (pick CameraTool >> refreshCamera)
    _ <- on filledB #toggled (Gtk.toggleButtonGetActive filledB >>= \f -> modifyIORef' st (\s -> s {filled = f}))
    _ <- on colourB #colorSet (getColour colourB >>= \c -> modifyIORef' st (\s -> s {colour = c}))
    _ <- on sizeS #valueChanged (Gtk.rangeGetValue sizeS >>= \v -> modifyIORef' st (\s -> s {penSize = v}))
    _ <- on alphaS #valueChanged (Gtk.rangeGetValue alphaS >>= \v -> modifyIORef' st (\s -> s {penAlpha = v}))
    -- The assistant's turns: the model asks for tools, they run here (each change one
    -- undoable edit), their outcomes go back, until it answers.
    let
        chatLine role body = do
            modifyIORef' chatRef (\c -> c {shownLines = c.shownLines <> [(role, body)]})
            Gtk.widgetSetVisible chatEmpty False
            w <- case role of
                _ | role `elem` ["step", "failed"] -> do
                    row <- Gtk.boxNew Gtk.OrientationHorizontal 6
                    img <- iconImage (if role == "failed" then "alert" else "step") 14
                    l <- Gtk.labelNew (Just body)
                    addClass l (if role == "failed" then "msg-failed" else "msg-step")
                    Gtk.labelSetXalign l 0
                    Gtk.labelSetLineWrap l True
                    Gtk.boxPackStart row img False False 0
                    Gtk.boxPackStart row l True True 0
                    toW row
                _ -> do
                    l <- Gtk.labelNew (Just body)
                    addClass l ("msg-" <> role)
                    Gtk.labelSetLineWrap l True
                    Gtk.labelSetLineWrapMode l Pango.WrapModeWordChar
                    Gtk.labelSetSelectable l True
                    Gtk.labelSetXalign l 0
                    when (role == "user") $ do
                        Gtk.widgetSetHalign l Gtk.AlignEnd
                        Gtk.labelSetMaxWidthChars l 30
                    toW l
            Gtk.boxPackStart chatList w False False 0
            Gtk.widgetShowAll w
            -- Down to the newest line once it is laid out.
            void . GLib.idleAdd GLib.PRIORITY_LOW $ do
                adj <- Gtk.scrolledWindowGetVadjustment chatScroll
                up <- Gtk.adjustmentGetUpper adj
                Gtk.adjustmentSetValue adj up
                pure False
        chatBusyShown b = do
            Gtk.widgetSetVisible stopB b
            Gtk.widgetSetSensitive sendB (not b)
        chatConnected = do
            c <- readIORef chatRef
            Gtk.labelSetText chatModelL (maybe "Not connected: connect a model" (.label) c.talker)
        systemPrompt = do
            s <- readIORef st
            pure $ case s.open of
                Nothing -> instructions
                Just o ->
                    instructions
                        <> "\n\nYou are the assistant inside the anima2D desktop editor, working on the open project \""
                        <> o.view.title
                        <> "\" ("
                        <> Text.pack (show o.view.width <> " × " <> show o.view.height <> ", " <> show o.view.rate <> " fps")
                        <> "). The person sees each change as you make it and can undo each one. editor_state tells you what they are looking at. After drawing, call preview to look at the frame and fix what is off. Keep replies short and plain."
        -- What a tool did, in a few words for the conversation.
        stepText (ToolUse _ nm args) = case (nm, args) of
            ("add_items", Aeson.Object o) -> "Drew " <> count (KeyMap.lookup "items" o) <> " on drawing " <> num (KeyMap.lookup "drawing" o)
            ("replace_items", Aeson.Object o) -> "Redrew drawing " <> num (KeyMap.lookup "drawing" o)
            ("add_drawing", _) -> "Added a drawing"
            ("add_layer", Aeson.Object o) | Just (Aeson.String n) <- KeyMap.lookup "name" o -> "Added the layer " <> n
            ("set_layer", _) -> "Changed a layer"
            ("add_light", _) -> "Added a light"
            ("set_camera", _) -> "Set the camera"
            ("preview", Aeson.Object o) -> "Looked at frame " <> maybe "1" (num . Just) (KeyMap.lookup "frame" o)
            ("preview", _) -> "Looked at frame 1"
            ("undo", _) -> "Undid the last change"
            ("redo", _) -> "Redid it"
            ("item_format", _) -> "Read the item format"
            ("editor_state", _) -> "Looked at what you are viewing"
            ("get_project", _) -> "Read the project"
            ("get_drawing", Aeson.Object o) -> "Read drawing " <> num (KeyMap.lookup "drawing" o)
            _ -> nm
            where
                num = \case
                    Just (Aeson.Number n) -> Text.pack (show (round n :: Int))
                    _ -> "?"
                count = \case
                    Just (Aeson.Array xs) -> Text.pack (show (length xs)) <> (if length xs == 1 then " item" else " items")
                    _ -> "items"
        -- One tool, on the GTK thread.
        runAssistantTool use@(ToolUse cid nm args) = do
            s <- readIORef st
            let
                answer t = pure (ToolOutcome cid t Nothing False)
                refuse why = chatLine "failed" (stepText use <> ": " <> why) >> pure (ToolOutcome cid why Nothing True)
                encodeT = TextEnc.decodeUtf8 . LBS.toStrict . Aeson.encode
            case (s.open, nm) of
                (Nothing, _) -> refuse "no project is open"
                (Just o, "editor_state") -> do
                    let
                        here = lookup s.layer (zip [0 ..] o.view.stack)
                        drawingNo = case here of
                            Just LayerView {kind = DrawingView holdLast cels} -> fmap (+ 1) (celAt holdLast cels s.playhead)
                            _ -> Nothing
                    chatLine "step" (stepText use)
                    answer . encodeT $ Aeson.object ["frame" Aeson..= (s.playhead + 1), "frames" Aeson..= duration o.view, "layer" Aeson..= fmap (.layerId) here, "layerName" Aeson..= fmap (.name) here, "drawing" Aeson..= drawingNo, "selectedItems" Aeson..= length s.selection]
                (Just o, "preview") -> do
                    let
                        number k = case args of
                            Aeson.Object a | Just (Aeson.Number n) <- KeyMap.lookup k a -> Just (round n :: Int)
                            _ -> Nothing
                        frame = fromMaybe 1 (number "frame")
                        width = fromMaybe 960 (number "width")
                    if frame < 1 || frame > duration o.view
                        then refuse ("expected a frame from 1 to " <> Text.pack (show (duration o.view)))
                        else
                            if width < 64 || width > 1920
                                then refuse "expected a width from 64 to 1920 pixels"
                                else do
                                    dir <- cacheFolder
                                    let
                                        file = dir </> "assistant-preview.png"
                                    renderPng o.env.pictures o.view (frame - 1) width file
                                    png <- BS.readFile file
                                    chatLine "step" (stepText use)
                                    pure (ToolOutcome cid ("Frame " <> Text.pack (show frame) <> " of " <> Text.pack (show (duration o.view)) <> ", as exported (white paper, through the camera).") (Just png) False)
                (Just _, "undo") -> do
                    if null s.past then refuse "nothing to undo" else undo >> chatLine "step" (stepText use) >> answer "Undone."
                (Just _, "redo") -> do
                    if null s.future then refuse "nothing to redo" else redo >> chatLine "step" (stepText use) >> answer "Redone."
                (Just o, _) -> case projectTool nm args of
                    Nothing -> refuse "there is no such tool"
                    Just (Left why) -> refuse why
                    Just (Right (Reads f)) -> case f o.project of
                        Left why -> refuse why
                        Right (Aeson.String t) -> chatLine "step" (stepText use) >> answer t
                        Right v -> chatLine "step" (stepText use) >> answer (encodeT v)
                    Just (Right (Changes f)) -> case f o.project of
                        Left why -> refuse why
                        Right p' -> do
                            commit (const (Right p'))
                            chatLine "step" (stepText use)
                            answer ("Done. The project has " <> Text.pack (show (duration (viewProject p'))) <> " frames now.")
        finishTurn = do
            modifyIORef' chatRef (\c -> c {busy = False, cancelled = False})
            chatBusyShown False
        sendChat raw = do
            c <- readIORef chatRef
            let
                text = Text.strip raw
            case c.talker of
                _ | Text.null text || c.busy -> pure ()
                Nothing -> chatLine "error" "Connect a model first: the gear above."
                Just m -> do
                    Gtk.stackSetVisibleChildName side "assistant"
                    chatLine "user" text
                    modifyIORef' chatRef (\x -> x {history = x.history <> m.userTurn text, busy = True, cancelled = False})
                    chatBusyShown True
                    let
                        loop :: Int -> IO ()
                        loop rounds = do
                            system <- onMainWait systemPrompt
                            sofar <- (.history) <$> readIORef chatRef
                            reply <- m.ask system assistantTools sofar
                            again <- onMainWait $ case reply of
                                Left why -> chatLine "error" why >> finishTurn >> pure False
                                Right r -> do
                                    modifyIORef' chatRef (\x -> x {history = x.history <> [r.keep]})
                                    unless (Text.null r.said) (chatLine "assistant" r.said)
                                    stopped <- (.cancelled) <$> readIORef chatRef
                                    case r.stop of
                                        WantsTools
                                            | stopped || rounds >= 40 -> do
                                                -- Every asked-for tool still gets an answer, so the conversation stays whole.
                                                let
                                                    why = if stopped then "Stopped by the person before it ran." else "Stopped: too many steps in one go."
                                                modifyIORef' chatRef (\x -> x {history = x.history <> m.resultTurn [ToolOutcome u.callId why Nothing True | u <- r.calls]})
                                                chatLine "error" why
                                                finishTurn
                                                pure False
                                            | otherwise -> do
                                                outs <- mapM runAssistantTool r.calls
                                                modifyIORef' chatRef (\x -> x {history = x.history <> m.resultTurn outs})
                                                pure True
                                        Done -> finishTurn >> pure False
                                        CutOff -> chatLine "error" "The answer was cut short: it ran out of room." >> finishTurn >> pure False
                                        Declined why -> chatLine "error" ("The model declined this." <> (if Text.null why then "" else " " <> why)) >> finishTurn >> pure False
                            when again (loop (rounds + 1))
                    void (forkIO (loop 0))
        useModel m = do
            modifyIORef' chatRef (\c -> c {talker = m})
            chatConnected
        -- The saved connection, at start or after the dialog: the key from the keychain.
        reconnect = do
            saved <- loadSettings
            forM_ saved $ \cfg -> background (loadKey (providerName cfg.provider)) $ \key -> case (cfg.provider, key) of
                (Anthropic, Nothing) -> Gtk.labelSetText chatModelL "Connect a model: the key is missing"
                _ -> connectModel cfg (maybe "" keyText key) >>= useModel . Just
    _ <- on sendB #clicked $ do
        buf <- Gtk.textViewGetBuffer composer
        (a, b) <- Gtk.textBufferGetBounds buf
        t <- Gtk.textBufferGetText buf a b False
        c <- readIORef chatRef
        unless (c.busy || Text.null (Text.strip t)) (Gtk.textBufferSetText buf "" (-1))
        sendChat t
    _ <- on stopB #clicked (modifyIORef' chatRef (\c -> c {cancelled = True}) >> Gtk.labelSetText chatModelL "Stopping after this step…")
    _ <- on composer #keyPressEvent $ \ev -> do
        kv <- get ev #keyval
        mods <- get ev #state
        name <- fromMaybe "" <$> Gdk.keyvalName kv
        if name `elem` ["Return", "KP_Enter"] && Gdk.ModifierTypeShiftMask `notElem` mods
            then Gtk.buttonClicked sendB >> pure True
            else pure False
    _ <- on chatNewB #clicked $ do
        c <- readIORef chatRef
        unless c.busy $ do
            writeIORef chatRef c {history = [], shownLines = []}
            kids <- Gtk.containerGetChildren chatList
            forM_ kids $ \k -> do
                isHint <- (==) <$> toW chatEmpty <*> pure k
                unless isHint (Gtk.widgetDestroy k)
            Gtk.widgetSetVisible chatEmpty True
    _ <- on chatSetB #clicked $ do
        saved <- loadSettings
        chosen <- connectDialog win saved
        forM_ chosen $ \(cfg, key) -> do
            saveSettings cfg
            stored <- case key of
                Just k -> saveKey (providerName cfg.provider) k
                Nothing -> pure (Right ())
            either say (const reconnect) stored
    reconnect
    -- Export: the choices, where to, then the frames in the background with a progress bar.
    let
        exportTo e out = do
            s <- readIORef st
            forM_ ((,) <$> s.backend <*> s.open) $ \(b, o) -> do
                stopping <- newIORef False
                d <- new Gtk.Dialog [#title := "Exporting", #modal := True, #deletable := False]
                Gtk.windowSetTransientFor d (Just win)
                area <- Gtk.dialogGetContentArea d
                bar <- Gtk.progressBarNew
                Gtk.progressBarSetShowText bar True
                Gtk.widgetSetSizeRequest bar 360 (-1)
                forM_ [Gtk.widgetSetMarginStart, Gtk.widgetSetMarginEnd, Gtk.widgetSetMarginTop, Gtk.widgetSetMarginBottom] $ \m -> m bar 18
                Gtk.containerAdd area bar
                _ <- Gtk.dialogAddButton d "Stop" (fromIntegral (fromEnum Gtk.ResponseTypeCancel))
                _ <- on d #response (\_ -> writeIORef stopping True)
                Gtk.widgetShowAll d
                say "Exporting…"
                cache <- newCache
                let
                    env = Env {pictures = o.env.pictures, cache, project = o.view}
                    progress n total = onMain $ do
                        Gtk.progressBarSetFraction bar (fromIntegral n / fromIntegral (max 1 total))
                        Gtk.progressBarSetText bar (Just (Text.pack ("Frame " <> show n <> " of " <> show total)))
                    run = do
                        tracks <- fmap catMaybes . forM [t | t <- o.view.soundtrack, not t.muted] $ \t ->
                            fmap (\path -> Track path t.offset t.volume) <$> b.picture t.asset
                        exportFilm (ffmpegPath frames) env tracks e out progress (readIORef stopping)
                background run $ \r -> do
                    Gtk.widgetDestroy d
                    case r of
                        Right () -> say ("Exported to " <> Text.pack out)
                        Left "stopped" -> say "Export stopped"
                        Left why -> say ("Not exported: " <> why)
    _ <- on exportBtn #clicked $ do
        s <- readIORef st
        forM_ s.open $ \o -> do
            chosen <- exportDialog win o.view
            forM_ chosen $ \e -> do
                out <- saveAs win o.view.title e.format
                forM_ out (exportTo e)
    -- The reference board: add, follow or play, take a colour, trace, remove; zoom and pan.
    lastStills <- newIORef (Map.empty :: Map.Map Text C.Surface)
    let
        addReferenceFile path = do
            s <- readIORef st
            forM_ s.backend $ \b -> do
                say "Adding to the board…"
                background (b.upload path) $ \case
                    Left why -> say why
                    Right asset -> do
                        let
                            video = map toLower (takeExtension path) `elem` [".mp4", ".m4v", ".mov", ".webm"]
                            base = Text.strip (Text.take 40 (Text.pack (takeBaseName path)))
                        rs <- boardRefs
                        modifyIORef' boardRef (\x -> x {showing = length rs})
                        commit (addReference (Aeson.object ["asset" Aeson..= asset, "media" Aeson..= (if video then "video" else "image" :: Text), "name" Aeson..= (if Text.null base then "Reference" else base), "follow" Aeson..= True]))
                        refreshPictures
                        Gtk.stackSetVisibleChildName side "board"
                        say "Added to the board"
        endOwnPlay = do
            modifyIORef' boardRef (\x -> x {ownPlay = Nothing})
            iconInto ownPlayB "play" "Play the video"
        -- What the board shows now: a picture, or the video's still (the last one while the
        -- next is being made).
        boardSurface = do
            cur <- currentRef
            s <- readIORef st
            b <- readIORef boardRef
            case (cur, s.open) of
                (Just r, Just o) -> do
                    found <- case r.media of
                        Image -> pure (Map.lookup r.asset o.env.pictures)
                        Video -> videoStill r.asset (if r.follows then fromIntegral s.playhead / fromIntegral (max 1 o.view.rate) else Map.findWithDefault 0 r.asset b.own)
                    case found of
                        Just sf -> modifyIORef' lastStills (Map.insert r.asset sf) >> pure (Just (r, sf))
                        Nothing -> fmap (r,) . Map.lookup r.asset <$> readIORef lastStills
                _ -> pure Nothing
        boardGeometry r sf = do
            w <- fromIntegral <$> Gtk.widgetGetAllocatedWidth boardView
            h <- fromIntegral <$> Gtk.widgetGetAllocatedHeight boardView
            sw <- fromIntegral <$> C.imageSurfaceGetWidth sf
            sh <- fromIntegral <$> C.imageSurfaceGetHeight sf
            b <- readIORef boardRef
            pure (w, h, boardPlace w h (Map.findWithDefault (1, 0, 0) r.asset b.views) (max 1 sw, max 1 sh))
    _ <- on boardAdd #clicked (chooseFile win >>= mapM_ addReferenceFile)
    _ <- on followB #toggled $ do
        syncing <- readIORef boardSyncing
        unless syncing $ do
            b <- readIORef boardRef
            following <- Gtk.toggleButtonGetActive followB
            stopOwnPlay
            commit (setReferenceFollow b.showing following)
    _ <- on pickB #toggled $ do
        syncing <- readIORef boardSyncing
        unless syncing $ Gtk.toggleButtonGetActive pickB >>= \picked -> modifyIORef' boardRef (\x -> x {picking = picked})
    _ <- on traceB #clicked $ do
        b <- readIORef boardRef
        commit (traceReference b.showing)
        modifyIORef' st (\x -> x {layer = x.layer + 1})
        say "Under the drawings as a locked layer at half strength"
    _ <- on removeRefB #clicked $ do
        b <- readIORef boardRef
        stopOwnPlay
        commit (removeReferenceAt b.showing)
    _ <- on ownPlayB #clicked $ do
        b <- readIORef boardRef
        case b.ownPlay of
            Just _ -> stopOwnPlay
            Nothing -> do
                -- Twelve stills a second: each is a moment ffmpeg cuts out.
                src <- GLib.timeoutSourceNew 83
                GLib.sourceSetCallback src $ do
                    cur <- currentRef
                    case cur of
                        Just r
                            | r.media == Video
                            , not r.follows -> do
                                path <- videoFile r.asset
                                len <- maybe (pure Nothing) (\pth -> lengthOf frames r.asset pth (pure ())) path
                                modifyIORef' boardRef $ \x ->
                                    let
                                        t = Map.findWithDefault 0 r.asset x.own + 1 / 12
                                    in
                                        x {own = Map.insert r.asset (maybe t (\l -> if t >= l then 0 else t) len) x.own}
                                x <- readIORef boardRef
                                forM_ len $ \l -> do
                                    writeIORef boardSyncing True
                                    Gtk.rangeSetValue scrubS (Map.findWithDefault 0 r.asset x.own / max 0.001 l)
                                    writeIORef boardSyncing False
                                Gtk.widgetQueueDraw boardView
                                pure True
                        _ -> endOwnPlay >> pure False
                _ <- GLib.sourceAttach src Nothing
                writeIORef boardRef b {ownPlay = Just src}
                iconInto ownPlayB "pause" "Pause the video"
    _ <- on scrubS #valueChanged $ do
        syncing <- readIORef boardSyncing
        cur <- currentRef
        case cur of
            Just r
                | not syncing
                , r.media == Video -> do
                    stopOwnPlay
                    v <- Gtk.rangeGetValue scrubS
                    path <- videoFile r.asset
                    len <- maybe (pure Nothing) (\pth -> lengthOf frames r.asset pth (onMain (Gtk.widgetQueueDraw boardView))) path
                    forM_ len $ \l -> modifyIORef' boardRef (\x -> x {own = Map.insert r.asset (v * l) x.own})
                    Gtk.widgetQueueDraw boardView
            _ -> pure ()
    _ <- on boardView #draw $ \ctx -> do
        found <- boardSurface
        geo <- traverse (\(r, sf) -> (sf,) <$> boardGeometry r sf) found
        Connector.renderWithContext
            ( do
                rgb workspace
                C.paint
                forM_ geo $ \(sf, (_, _, (x, y, k))) -> do
                    C.save
                    C.translate x y
                    C.scale k k
                    C.setSourceSurface sf 0 0
                    C.paint
                    C.restore
            )
            ctx
        pure True
    _ <- on boardView #event $ \ev -> do
        kind <- Gdk.eventGetEventType ev
        (_, x, y) <- Gdk.eventGetCoords ev
        found <- boardSurface
        b <- readIORef boardRef
        case (kind, found) of
            (Gdk.EventType2buttonPress, Just (r, _)) -> do
                writeIORef boardRef b {views = Map.delete r.asset b.views, panFrom = Nothing}
                Gtk.widgetQueueDraw boardView
                pure True
            (Gdk.EventTypeButtonPress, Just (r, sf))
                | b.picking -> do
                    (_, _, (ox, oy, k)) <- boardGeometry r sf
                    picked <- pixelAt sf (floor ((x - ox) / k)) (floor ((y - oy) / k))
                    forM_ picked $ \hex -> do
                        setColour colourB hex
                        modifyIORef' st (\s' -> s' {colour = hex})
                        modifyIORef' boardRef (\x' -> x' {picking = False})
                        say ("Colour " <> hex)
                        refreshBoard
                    pure True
                | otherwise -> do
                    let
                        (_, vx, vy) = Map.findWithDefault (1, 0, 0) r.asset b.views
                    writeIORef boardRef b {panFrom = Just (x, y, vx, vy)}
                    pure True
            (Gdk.EventTypeMotionNotify, Just (r, _)) | Just (x0, y0, vx, vy) <- b.panFrom -> do
                let
                    (vk, _, _) = Map.findWithDefault (1, 0, 0) r.asset b.views
                writeIORef boardRef b {views = Map.insert r.asset (vk, vx + x - x0, vy + y - y0) b.views}
                Gtk.widgetQueueDraw boardView
                pure True
            (Gdk.EventTypeButtonRelease, _) -> writeIORef boardRef b {panFrom = Nothing} >> pure True
            _ -> pure False
    _ <- on boardView #scrollEvent $ \ev -> do
        dy <- get ev #deltaY
        dir <- get ev #direction
        x <- get ev #x
        y <- get ev #y
        found <- boardSurface
        forM_ found $ \(r, _) -> do
            w <- fromIntegral <$> Gtk.widgetGetAllocatedWidth boardView
            h <- fromIntegral <$> Gtk.widgetGetAllocatedHeight boardView
            b <- readIORef boardRef
            let
                (vk, vx, vy) = Map.findWithDefault (1, 0, 0) r.asset b.views
                amount = case dir of
                    Gdk.ScrollDirectionUp -> -1
                    Gdk.ScrollDirectionDown -> 1
                    _ -> dy / 3
                k' = max 0.2 (min 40 (vk * exp (-(amount * 0.15))))
                f = k' / vk
                px = x - w / 2
                py = y - h / 2
            writeIORef boardRef b {views = Map.insert r.asset (k', px - (px - vx) * f, py - (py - vy) * f) b.views}
        Gtk.widgetQueueDraw boardView
        pure True
    -- The canvas: drawn from the open project and the stroke in progress.
    _ <- on canvas #draw $ \ctx -> do
        w <- fromIntegral <$> Gtk.widgetGetAllocatedWidth canvas
        h <- fromIntegral <$> Gtk.widgetGetAllocatedHeight canvas
        s0 <- readIORef st
        s <- case s0.open of
            Just o | not s0.fitted -> do
                let
                    k = max 0.05 (min ((w - 48) / fromIntegral o.view.width) ((h - 48) / fromIntegral o.view.height))
                    s' = s0 {magnify = k, panX = (w - fromIntegral o.view.width * k) / 2, panY = (h - fromIntegral o.view.height * k) / 2, fitted = True}
                writeIORef st s'
                pure s'
            _ -> pure s0
        under <- case s.open of
            Nothing -> pure []
            Just o -> fmap catMaybes . forM [(l, a, m) | l@LayerView {kind = ReferenceView a m} <- o.view.stack, l.visible] $ \(l, a, m) -> case m of
                Image -> pure ((l.opacity,) <$> Map.lookup a o.env.pictures)
                Video -> fmap (l.opacity,) <$> videoStill a (fromIntegral s.playhead / fromIntegral (max 1 o.view.rate))
        Connector.renderWithContext (drawCanvas s under (finish s)) ctx
        pure True
    -- A canvas that changes size fits the paper again.
    lastSize <- newIORef (0, 0)
    _ <- on canvas #sizeAllocate $ \_ -> do
        cw <- Gtk.widgetGetAllocatedWidth canvas
        ch <- Gtk.widgetGetAllocatedHeight canvas
        was <- readIORef lastSize
        unless (was == (cw, ch)) $ do
            writeIORef lastSize (cw, ch)
            modifyIORef' st (\s -> s {fitted = False})
    -- Pen, mouse and touch in one place: the pen and the mouse draw (with pressure where there
    -- is any); touch, the middle button and Space-drag pan.
    let
        onPointer pe = do
            s <- readIORef st
            let
                (x, y) = (pe.px, pe.py)
                pressure = pe.pressure
                button = pe.button
                panning = pe.panning || button == 2 || s.spaceHeld
            case pe.phase of
                Press
                    | panning -> writeIORef st s {drag = Just (x, y, s.panX, s.panY)} >> pure True
                    | button == 1 -> do
                        Gtk.widgetGrabFocus canvas
                        stop
                        t <- target
                        case (t, s.tool) of
                            (_, CameraTool) -> do
                                here <- shotHere
                                forM_ here $ \(_, k) -> writeIORef st s {camDrag = Just (canvasPoint s (x, y), k.shotCentre)}
                                pure True
                            (Left why, _) -> say why >> pure True
                            (Right _, TextTool) -> do
                                body <- askText win "Text" ""
                                forM_ body $ \b -> celEdit (\li n p -> Make.textAt (pen s) b (max 24 (s.penSize * 6)) (toLayer s (x, y)) >>= \item -> addItemsAt li n [item] p)
                                pure True
                            (Right _, EraserTool) | s.objectEraser -> do
                                eraseAt (toLayer s (x, y)) s
                                pure True
                            (Right (o, _, n), FillTool) -> do
                                let
                                    (lx, ly) = toLayer s (x, y)
                                    items = celItems o.view s.layer n
                                raster <- Fill.rasterOf o.env.pictures o.view.width o.view.height items
                                case Fill.traceFill raster lx ly 4 maxPointsPerFill >>= Make.fillArea (pen s) of
                                    Left why -> say why
                                    Right item -> celEdit (\li m -> addItemsAt li m [item])
                                pure True
                            (Right (o, _, n), SelectTool) -> do
                                let
                                    p@(lx, ly) = toLayer s (x, y)
                                    items = celItems o.view s.layer n
                                    chosen = [it | (i, it) <- zip [0 ..] items, i `elem` s.selection]
                                    pad = 12 / s.magnify
                                    nearP (ax, ay) = abs (ax - lx) <= pad && abs (ay - ly) <= pad
                                    startDrag m c = writeIORef st s {selDrag = Just SelDrag {mode = m, from = p, centre = c, current = M.identity}}
                                case boxOf chosen of
                                    Just bx
                                        | any nearP [(bx.x0, bx.y0), (bx.x1, bx.y0), (bx.x1, bx.y1), (bx.x0, bx.y1)] -> startDrag Scaling (middle bx)
                                        | nearP ((bx.x0 + bx.x1) / 2, bx.y0 - 30 / s.magnify) -> startDrag Turning (middle bx)
                                        | lx >= bx.x0 && lx <= bx.x1 && ly >= bx.y0 && ly <= bx.y1 -> startDrag Moving (middle bx)
                                    _ -> case topmostAt items lx ly (6 / s.magnify) of
                                        Just i -> do
                                            let
                                                sel = if s.shiftHeld then (if i `elem` s.selection then filter (/= i) s.selection else i : s.selection) else [i]
                                                box = boxOf [it | (j, it) <- zip [0 ..] items, j `elem` sel]
                                            writeIORef st s {selection = sel, selDrag = fmap (\bx -> SelDrag {mode = Moving, from = p, centre = middle bx, current = M.identity}) box}
                                        Nothing -> writeIORef st s {selection = []}
                                Gtk.widgetQueueDraw canvas
                                pure True
                            (Right _, _) -> do
                                let
                                    (lx, ly) = toLayer s (x, y)
                                writeIORef st s {draft = Just Draft {pts = [(lx, ly, pressure)], start = (lx, ly), seed = Make.seedFrom lx ly (length s.past)}}
                                Gtk.widgetQueueDraw canvas
                                pure True
                    | otherwise -> pure False
                Move -> do
                    case (s.drag, s.draft) of
                        (Just (x0, y0, px, py), _) -> writeIORef st s {panX = px + x - x0, panY = py + y - y0} >> Gtk.widgetQueueDraw canvas
                        _ | Just (p0, c0) <- s.camDrag -> do
                            let
                                (px, py) = canvasPoint s (x, y)
                            setShot "move" (\k -> k {shotCentre = bimap (+ (px - fst p0)) (+ (py - snd p0)) c0})
                        _ | Just sd <- s.selDrag -> do
                            let
                                (lx, ly) = toLayer s (x, y)
                                (fx, fy) = sd.from
                                (cx, cy) = sd.centre
                                about (M.Matrix a b c d e f) = M.Matrix a b c d (e + cx - a * cx - c * cy) (f + cy - b * cx - d * cy)
                                m' = case sd.mode of
                                    Moving -> M.translate (lx - fx) (ly - fy) M.identity
                                    Scaling ->
                                        let
                                            k = sqrt ((lx - cx) ** 2 + (ly - cy) ** 2) / max 1.0e-6 (sqrt ((fx - cx) ** 2 + (fy - cy) ** 2))
                                        in
                                            about (M.scale k k M.identity)
                                    Turning ->
                                        let
                                            a0 = atan2 (ly - cy) (lx - cx) - atan2 (fy - cy) (fx - cx)
                                            a = if s.shiftHeld then fromIntegral (round (a0 / (pi / 12)) :: Int) * (pi / 12) else a0
                                        in
                                            about (M.rotate a M.identity)
                            writeIORef st s {selDrag = Just sd {current = m'}}
                            Gtk.widgetQueueDraw canvas
                        (_, Just d) -> do
                            let
                                (lx, ly) = toLayer s (x, y)
                            writeIORef st s {draft = Just d {pts = d.pts <> [(lx, ly, pressure)]}}
                            Gtk.widgetQueueDraw canvas
                        _ -> when (s.tool == EraserTool && s.objectEraser) $ do
                            -- The object eraser rubs out what it passes over while pressed.
                            when pe.held (eraseAt (toLayer s (x, y)) s)
                    pure True
                Release -> do
                    writeIORef st s {drag = Nothing, draft = Nothing, selDrag = Nothing, camDrag = Nothing}
                    when (isJust s.camDrag) (modifyIORef' st (\x' -> x' {lastKey = Nothing}))
                    forM_ s.selDrag $ \sd -> unless (sd.current == M.identity || null s.selection) $ do
                        let
                            M.Matrix a b c d e f = sd.current
                        celEdit (\li n -> transformItemsAt li n s.selection (a, b, c, d, e, f))
                    forM_ s.draft $ \d -> forM_ (finish s d) $ \case
                        Left why -> say why
                        Right item -> celEdit (\li n -> addItemsAt li n [item])
                    Gtk.widgetQueueDraw canvas
                    pure True
    _ <- on canvas #event $ \ev -> do
        kind <- Gdk.eventGetEventType ev
        (_, x, y) <- Gdk.eventGetCoords ev
        (hasP, pr) <- Gdk.eventGetAxis ev Gdk.AxisUsePressure
        (_, button) <- Gdk.eventGetButton ev
        src <- Gdk.eventGetSourceDevice ev >>= maybe (pure Gdk.InputSourceMouse) Gdk.deviceGetSource
        (_, mods) <- Gdk.eventGetState (Just ev)
        let
            send ph = onPointer Pointer {phase = ph, px = x, py = y, pressure = if hasP && pr > 0 then pr else 0.5, button = fromIntegral button, panning = src == Gdk.InputSourceTouchscreen, held = Gdk.ModifierTypeButton1Mask `elem` mods}
        case kind of
            Gdk.EventTypeButtonPress -> send Press
            Gdk.EventTypeMotionNotify -> send Move
            Gdk.EventTypeButtonRelease -> send Release
            _ -> pure False
    -- Scrolling: a mouse wheel zooms about the pointer; a trackpad pans, and zooms with Ctrl or ⌘.
    _ <- on canvas #scrollEvent $ \ev -> do
        dx <- get ev #deltaX
        dy <- get ev #deltaY
        dir <- get ev #direction
        x <- get ev #x
        y <- get ev #y
        mods <- get ev #state
        let
            zoomBy amount = modifyIORef' st $ \s ->
                let
                    z = max 0.05 (min 32 (s.magnify * exp (-(amount * 0.12))))
                    r = z / s.magnify
                in
                    s {magnify = z, panX = x - (x - s.panX) * r, panY = y - (y - s.panY) * r}
            ctrl = any (`elem` mods) [Gdk.ModifierTypeControlMask, Gdk.ModifierTypeMetaMask, Gdk.ModifierTypeSuperMask]
        case dir of
            Gdk.ScrollDirectionUp -> zoomBy (-1)
            Gdk.ScrollDirectionDown -> zoomBy 1
            _
                | ctrl -> zoomBy (dy / 3)
                | otherwise -> modifyIORef' st (\s -> s {panX = s.panX - dx * 12, panY = s.panY - dy * 12})
        Gtk.widgetQueueDraw canvas
        pure True
    -- The camera's bar: each control sets the key at the playhead.
    _ <- on camZoom #valueChanged (Gtk.rangeGetValue camZoom >>= \z -> setShot "zoom" (\k -> k {shotZoom = z}))
    _ <- on camTurn #valueChanged (Gtk.rangeGetValue camTurn >>= \a -> setShot "turn" (\k -> k {shotTurn = a * pi / 180}))
    _ <- on camShake #valueChanged (Gtk.rangeGetValue camShake >>= \x -> setShot "shake" (\k -> k {shotShake = x}))
    _ <- on camAperture #valueChanged (Gtk.rangeGetValue camAperture >>= \x -> setShot "aperture" (\k -> k {shotBlur = x}))
    _ <- on camFocus #valueChanged (Gtk.rangeGetValue camFocus >>= \x -> setShot "focus" (\k -> k {shotFocus = x}))
    _ <- on camTintA #valueChanged (Gtk.rangeGetValue camTintA >>= \x -> setShot "tintAmount" (\k -> k {shotTintAmount = x}))
    _ <- on camTintB #toggled $ do
        on' <- Gtk.toggleButtonGetActive camTintB
        c <- getColour camTintC
        setShot "tint" (\k -> k {shotTint = if on' then Just c else Nothing, shotTintAmount = if on' && k.shotTintAmount == 0 then 0.25 else k.shotTintAmount})
    _ <- on camTintC #colorSet (getColour camTintC >>= \c -> setShot "tint" (\k -> k {shotTint = Just c}))
    _ <- on camEase #changed (Gtk.comboBoxGetActiveId camEase >>= \e -> setShot "ease" (\k -> k {shotEase = easeFromId e}))
    _ <- on camRemove #clicked $ do
        s <- readIORef st
        forM_ s.open $ \o -> do
            commit (setCamera (Aeson.toJSON [shotValue (shotOf k) | k <- o.view.shots, k.frame /= s.playhead]))
            refreshCamera
    -- The layer panel: a layer's name, opacity, blend, clipping, depth, effects and, for a
    -- light, its light; and its place in the stack. Opened from the timeline's layer names.
    panelLayer <- newIORef (0 :: Int)
    panelSyncing <- newIORef False
    panel <- Gtk.popoverNew (Just timeline)
    Gtk.popoverSetPosition panel Gtk.PositionTypeRight
    pGrid <- new Gtk.Grid [#rowSpacing := 8, #columnSpacing := 10, #margin := 14]
    pName <- Gtk.entryNew
    pOpacity <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal 0 1 0.01
    pBlend <- Gtk.comboBoxTextNew
    forM_ [minBound .. maxBound :: Blend] $ \bl -> Gtk.comboBoxTextAppend pBlend (Just (blendName bl)) (Text.toTitle (blendName bl))
    pClip <- Gtk.checkButtonNewWithLabel "Clip to the layer below"
    pDepth <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal 0.2 20 0.1
    pShadow <- Gtk.checkButtonNewWithLabel "Drop shadow"
    pShadowC <- Gtk.colorButtonNew
    pRim <- Gtk.checkButtonNewWithLabel "Rim light"
    pRimC <- Gtk.colorButtonNew
    pLightC <- Gtk.colorButtonNew
    pIntensity <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal 0 1 0.01
    pRadius <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal 10 4000 10
    pSoft <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal 0 1 0.01
    pUp <- iconButton "up" "Move up"
    pDown <- iconButton "down" "Move down"
    pDelete <- iconButton "remove" "Delete layer"
    forM_ [pOpacity, pDepth, pIntensity, pRadius, pSoft] $ \sc -> do
        Gtk.scaleSetValuePos sc Gtk.PositionTypeRight
        Gtk.widgetSetSizeRequest sc 170 (-1)
    Gtk.scaleSetDigits pDepth 1
    Gtk.scaleSetDigits pRadius 0
    effectsRow <- Gtk.boxNew Gtk.OrientationHorizontal 8
    pack effectsRow [toW pShadow, toW pShadowC, toW pRim, toW pRimC]
    moveRow <- Gtk.boxNew Gtk.OrientationHorizontal 2
    pack moveRow [toW pUp, toW pDown]
    Gtk.boxPackEnd moveRow pDelete False False 0
    lightLabels <- forM ["Light", "Intensity", "Radius", "Softness"] (Gtk.labelNew . Just)
    let
        rows = [("Name", toW pName), ("Opacity", toW pOpacity), ("Blend", toW pBlend), ("", toW pClip), ("Depth", toW pDepth), ("Effects", toW effectsRow)]
    forM_ (zip [0 ..] rows) $ \(r, (lbl, mk)) -> do
        l <- Gtk.labelNew (Just lbl)
        addClass l "dim"
        Gtk.labelSetXalign l 1
        w <- mk
        Gtk.gridAttach pGrid l 0 r 1 1
        Gtk.gridAttach pGrid w 1 r 1 1
    forM_ (zip3 [6 ..] lightLabels [toW pLightC, toW pIntensity, toW pRadius, toW pSoft]) $ \(r, l, mk) -> do
        addClass l "dim"
        Gtk.labelSetXalign l 1
        w <- mk
        Gtk.gridAttach pGrid l 0 r 1 1
        Gtk.gridAttach pGrid w 1 r 1 1
    Gtk.gridAttach pGrid moveRow 0 10 2 1
    Gtk.containerAdd panel pGrid
    lightWidgets <- (<>) <$> mapM toW lightLabels <*> sequence [toW pLightC, toW pIntensity, toW pRadius, toW pSoft]
    let
        panelView = do
            s <- readIORef st
            li <- readIORef panelLayer
            pure (s.open >>= \o -> lookup li (zip [0 ..] o.view.stack))
        -- One setting of the panel's layer, as the library's restyle takes it.
        setLayer key val = do
            syncing <- readIORef panelSyncing
            li <- readIORef panelLayer
            unless syncing $ commitKeyed (key <> Text.pack (show li)) (restyleLayer li (KeyMap.fromList [(Key.fromText key, val)]))
        effectsNow = do
            mine <- panelView
            sh <- Gtk.toggleButtonGetActive pShadow
            ri <- Gtk.toggleButtonGetActive pRim
            shC <- getColour pShadowC
            riC <- getColour pRimC
            let
                was = mine
                shadowJ = case was >>= (.shadow) of
                    Just x -> Aeson.object ["color" Aeson..= shC, "opacity" Aeson..= x.opacity, "offset" Aeson..= [fst x.offset, snd x.offset], "blur" Aeson..= x.blur]
                    Nothing -> Aeson.object ["color" Aeson..= shC, "opacity" Aeson..= (0.35 :: Double), "offset" Aeson..= [14 :: Double, 22], "blur" Aeson..= (14 :: Double)]
                rimJ = case was >>= (.rim) of
                    Just x -> Aeson.object ["color" Aeson..= riC, "opacity" Aeson..= x.opacity, "width" Aeson..= x.width, "angle" Aeson..= x.angle]
                    Nothing -> Aeson.object ["color" Aeson..= riC, "opacity" Aeson..= (0.85 :: Double), "width" Aeson..= (6 :: Double), "angle" Aeson..= (-2.36 :: Double)]
            pure
                $ if sh || ri
                    then Aeson.object (["shadow" Aeson..= shadowJ | sh] <> ["rim" Aeson..= rimJ | ri])
                    else Aeson.Null
        lightNow = do
            mine <- panelView
            c <- getColour pLightC
            i <- Gtk.rangeGetValue pIntensity
            r <- Gtk.rangeGetValue pRadius
            so <- Gtk.rangeGetValue pSoft
            pure $ case mine of
                Just LayerView {kind = LightLayerView lv} -> Just (Aeson.object ["shape" Aeson..= (if lv.shape == LinearLight then "linear" else "radial" :: Text), "color" Aeson..= c, "intensity" Aeson..= i, "center" Aeson..= [fst lv.center, snd lv.center], "radius" Aeson..= r, "angle" Aeson..= lv.angle, "softness" Aeson..= so])
                _ -> Nothing
        -- The controls from the layer as it is now.
        fillPanel = do
            mine <- panelView
            forM_ mine $ \l -> do
                writeIORef panelSyncing True
                Gtk.entrySetText pName l.name
                Gtk.rangeSetValue pOpacity l.opacity
                _ <- Gtk.comboBoxSetActiveId pBlend (Just (blendName l.mixing))
                Gtk.toggleButtonSetActive pClip l.clipped
                Gtk.rangeSetValue pDepth l.distance
                Gtk.toggleButtonSetActive pShadow (isJust l.shadow)
                Gtk.toggleButtonSetActive pRim (isJust l.rim)
                setColour pShadowC (maybe "#1c1c1e" (.color) l.shadow)
                setColour pRimC (maybe "#ffd27a" (.color) l.rim)
                case l.kind of
                    LightLayerView lv -> do
                        mapM_ (`Gtk.widgetSetVisible` True) lightWidgets
                        setColour pLightC lv.color
                        Gtk.rangeSetValue pIntensity lv.intensity
                        Gtk.rangeSetValue pRadius lv.radius
                        Gtk.rangeSetValue pSoft lv.softness
                    _ -> mapM_ (`Gtk.widgetSetVisible` False) lightWidgets
                writeIORef panelSyncing False
        openPanel li = do
            s <- readIORef st
            forM_ s.open $ \o -> do
                writeIORef panelLayer li
                let
                    row = fromMaybe 0 (lookup li (zip (map fst (reverse (zip [0 ..] o.view.stack))) [0 :: Int ..]))
                rect <- new Gdk.Rectangle [#x := 0, #y := round (rulerH + fromIntegral row * rowH), #width := round headerW, #height := round rowH]
                Gtk.popoverSetPointingTo panel rect
                Gtk.widgetShowAll pGrid
                fillPanel
                Gtk.popoverPopup panel
                modifyIORef' st (\x -> x {layer = li})
                redraw
        shotSet k x = case k of
            "zoom" -> Gtk.rangeSetValue camZoom x
            "turn" -> Gtk.rangeSetValue camTurn x
            "shake" -> Gtk.rangeSetValue camShake x
            _ -> pure ()
        -- A control set from code, as a person would set it (for the UI test).
        panelSet key val = case (key, val) of
            ("name", Aeson.String t) -> Gtk.entrySetText pName t >> Gtk.widgetActivate pName >> pure ()
            ("opacity", Aeson.Number n) -> Gtk.rangeSetValue pOpacity (realToFrac n)
            ("depth", Aeson.Number n) -> Gtk.rangeSetValue pDepth (realToFrac n)
            ("blend", Aeson.String t) -> void (Gtk.comboBoxSetActiveId pBlend (Just t))
            ("clip", Aeson.Bool b) -> Gtk.toggleButtonSetActive pClip b
            ("shadow", Aeson.Bool b) -> Gtk.toggleButtonSetActive pShadow b
            ("up", _) -> Gtk.buttonClicked pUp
            ("down", _) -> Gtk.buttonClicked pDown
            _ -> pure ()
    _ <- on pName #activate (Gtk.entryGetText pName >>= setLayer "name" . Aeson.String)
    _ <- on pOpacity #valueChanged (Gtk.rangeGetValue pOpacity >>= setLayer "opacity" . Aeson.toJSON)
    _ <- on pDepth #valueChanged (Gtk.rangeGetValue pDepth >>= setLayer "depth" . Aeson.toJSON)
    _ <- on pBlend #changed (Gtk.comboBoxGetActiveId pBlend >>= mapM_ (setLayer "blend" . Aeson.String))
    _ <- on pClip #toggled (Gtk.toggleButtonGetActive pClip >>= setLayer "clip" . Aeson.Bool)
    forM_ [pShadow, pRim] $ \b -> on b #toggled (effectsNow >>= setLayer "effects")
    forM_ [pShadowC, pRimC] $ \b -> on b #colorSet (effectsNow >>= setLayer "effects")
    _ <- on pLightC #colorSet (lightNow >>= mapM_ (setLayer "light"))
    forM_ [pIntensity, pRadius, pSoft] $ \sc -> on sc #valueChanged (lightNow >>= mapM_ (setLayer "light"))
    forM_ [(pUp, 1), (pDown, -1)] $ \(b, by) -> on b #clicked $ do
        li <- readIORef panelLayer
        s <- readIORef st
        let
            count = maybe 0 (\o -> length o.view.stack) s.open
            to = max 0 (min (count - 1) (li + by))
        unless (to == li) $ do
            commit (moveLayer li by)
            writeIORef panelLayer to
            modifyIORef' st (\x -> x {layer = to})
            openPanel to
    _ <- on pDelete #clicked $ do
        li <- readIORef panelLayer
        Gtk.popoverPopdown panel
        commit (removeLayerAt li)
        modifyIORef' st (\x -> x {layer = max 0 (x.layer - 1)})
        redraw
    -- The timeline: click to move the playhead (and pick the layer); wheel scrolls it.
    _ <- on timeline #draw $ \ctx -> do
        w <- fromIntegral <$> Gtk.widgetGetAllocatedWidth timeline
        h <- fromIntegral <$> Gtk.widgetGetAllocatedHeight timeline
        s <- readIORef st
        Connector.renderWithContext (drawTimeline s w h) ctx
        pure True
    _ <- on timeline #buttonPressEvent $ \ev -> do
        x <- get ev #x
        y <- get ev #y
        kindOf <- Gdk.getEventButtonType ev
        button <- get ev #button
        s <- readIORef st
        forM_ s.open $ \o -> do
            let
                f = max 0 (min (duration o.view - 1) (floor ((x - headerW + s.tlScroll) / unitW)))
                row = floor ((y - rulerH) / rowH) :: Int
                layers = reverse (zip [0 ..] o.view.stack)
                picked = if y > rulerH then fmap fst (lookup row (zip [0 ..] layers)) else Nothing
            stop
            writeIORef st s {playhead = if x > headerW then f else s.playhead, layer = fromMaybe s.layer picked}
            redraw
            -- A double click or a right click on a layer's name opens its panel.
            when (x <= headerW && (kindOf == Gdk.EventType2buttonPress || button == 3)) (mapM_ openPanel picked)
        pure True
    _ <- on timeline #scrollEvent $ \ev -> do
        dx <- get ev #deltaX
        dy <- get ev #deltaY
        modifyIORef' st (\s -> s {tlScroll = max 0 (s.tlScroll + (dx + dy) * 20)})
        Gtk.widgetQueueDraw timeline
        pure True
    -- Keys, as in the browser: tools, frames, drawings, undo.
    let
        -- A command from the menu bar and from the window's own keys at once (⌘Z reaches
        -- both on some systems) runs once.
        guarded name act = do
            now <- getMonotonicTime
            (was, at) <- readIORef lastCommand
            unless (was == name && now - at < 0.2) $ do
                writeIORef lastCommand (name, now)
                act
        onKey name cmd shift = do
            s <- readIORef st
            if isNothing s.open
                then pure False
                else do
                    case (cmd, Text.toLower name) of
                        (True, "z") -> (if shift then guarded "redo" redo else guarded "undo" undo) >> pure True
                        (False, "v") -> Gtk.toggleButtonSetActive selectR True >> pure True
                        (False, "k") -> Gtk.toggleButtonSetActive fillR True >> pure True
                        (False, "escape") -> writeIORef st s {selection = []} >> Gtk.widgetQueueDraw canvas >> pure True
                        (False, n)
                            | n `elem` ["delete", "backspace"]
                            , not (null s.selection) -> do
                                writeIORef st s {selection = []}
                                celEdit (\li m p -> foldr (\k acc -> acc >>= removeItemAt li m k) (Right p) (sortDesc s.selection))
                                pure True
                        (True, "a") -> do
                            guarded "select-all" $ do
                                t <- target
                                forM_ t $ \(o, _, n) -> do
                                    writeIORef st s {selection = [i | (i, it) <- zip [0 ..] (celItems o.view s.layer n), not (isEraser it)]}
                                    Gtk.toggleButtonSetActive selectR True
                                    Gtk.widgetQueueDraw canvas
                            pure True
                        (False, "b") -> Gtk.toggleButtonSetActive brushR True >> pure True
                        (False, "g") -> Gtk.toggleButtonSetActive scatterR True >> pure True
                        (False, "e") -> Gtk.toggleButtonSetActive eraserR True >> pure True
                        (False, "u") -> Gtk.toggleButtonSetActive shapeR True >> pure True
                        (False, "t") -> Gtk.toggleButtonSetActive textR True >> pure True
                        (False, "c") -> Gtk.toggleButtonSetActive cameraR True >> pure True
                        (False, "o") -> Gtk.toggleButtonSetActive onionBtn (not s.onion) >> pure True
                        (False, "n") -> Gtk.buttonClicked newCelB >> pure True
                        (False, "d") -> Gtk.buttonClicked dupCelB >> pure True
                        (False, "left") -> stop >> step (-1) >> pure True
                        (False, "right") -> stop >> step 1 >> pure True
                        (False, "bracketleft") -> Gtk.rangeSetValue sizeS (max 0.5 (s.penSize / 1.25)) >> pure True
                        (False, "bracketright") -> Gtk.rangeSetValue sizeS (min 200 (s.penSize * 1.25)) >> pure True
                        (False, "0") -> Gtk.buttonClicked fitBtn >> pure True
                        (False, "shift_l") -> writeIORef st s {shiftHeld = True} >> pure False
                        (False, "shift_r") -> writeIORef st s {shiftHeld = True} >> pure False
                        (False, "space") -> do
                            -- Space plays; held while dragging, it pans.
                            writeIORef st s {spaceHeld = True}
                            pure True
                        _ -> pure False
        onKeyUp name = do
            s <- readIORef st
            case Text.toLower name of
                "space" -> do
                    writeIORef st s {spaceHeld = False}
                    when (isNothing s.drag) play
                    pure True
                n | n `elem` ["shift_l", "shift_r"] -> writeIORef st s {shiftHeld = False} >> pure False
                _ -> pure False
    let
        -- Typing in a text field (the assistant's, a dialog's) is not the editor's shortcuts.
        typing = do
            focused <- Gtk.windowGetFocus win
            case focused of
                Nothing -> pure False
                Just f -> (\a b -> isJust a || isJust b) <$> castTo Gtk.TextView f <*> castTo Gtk.Entry f
    _ <- on win #keyPressEvent $ \ev -> do
        kv <- get ev #keyval
        mods <- get ev #state
        name <- fromMaybe "" <$> Gdk.keyvalName kv
        intoText <- typing
        if intoText then pure False else onKey name (any (`elem` mods) [Gdk.ModifierTypeControlMask, Gdk.ModifierTypeMetaMask, Gdk.ModifierTypeSuperMask]) (Gdk.ModifierTypeShiftMask `elem` mods)
    _ <- on win #keyReleaseEvent $ \ev -> do
        kv <- get ev #keyval
        name <- fromMaybe "" <$> Gdk.keyvalName kv
        intoText <- typing
        if intoText then pure False else onKeyUp name
    _ <- on win #focusOutEvent $ \_ -> modifyIORef' st (\s -> s {spaceHeld = False, shiftHeld = False}) >> pure False
    Gtk.widgetShowAll win
    showOptions BrushTool
    chrome False
    let
        probe = do
            onBoard <- boardSurface
            chatNow <- readIORef chatRef
            s <- readIORef st
            msg <- Gtk.labelGetText saveState
            pure
                Probe
                    { edited = (.project) <$> s.open
                    , toolName = Text.toLower (Text.pack (takeWhile (/= 'T') (show s.tool)))
                    , chosen = s.selection
                    , undoSteps = length s.past
                    , unsaved = s.dirty
                    , note = msg
                    , onLayer = s.layer
                    , atFrame = s.playhead
                    , screenOf = \(cx, cy) -> (s.panX + cx * s.magnify, s.panY + cy * s.magnify)
                    , boardShows = isJust onBoard
                    , chatLog = chatNow.shownLines
                    , chatBusy = chatNow.busy
                    }
        buttons = [("+ Drawing", newCelB), ("Duplicate", dupCelB), ("Delete drawing", delCelB), ("Hold +", moreB), ("Hold −", lessB), ("+ Layer", addLayerB), ("Hide layer", hideB), ("Lock layer", lockB), ("Delete layer", delLayerB), ("Undo", undoBtn), ("Redo", redoBtn), ("Play", playBtn), ("◀", prevBtn), ("▶", nextBtn), ("Fit", fitBtn), ("Trace", traceB), ("Remove reference", removeRefB)]
        clickNamed n = case n of
            "Follow" -> Gtk.buttonClicked followB
            _ -> forM_ (lookup n buttons) Gtk.buttonClicked
    let
        runCommand name = do
            s <- readIORef st
            let
                editing = isJust s.open
                tool r = when editing (Gtk.toggleButtonSetActive r True)
            case name of
                "settings" -> Gtk.buttonClicked connBtn
                "connect-model" -> Gtk.buttonClicked chatSetB
                "new-project" -> Gtk.buttonClicked newBtn
                "projects" -> when editing (Gtk.buttonClicked backBtn)
                "export" -> when editing (Gtk.buttonClicked exportBtn)
                "undo" -> guarded "undo" undo
                "redo" -> guarded "redo" redo
                "select-all" -> void (onKey "a" True False)
                "delete" -> void (onKey "delete" False False)
                "fit" -> when editing (Gtk.buttonClicked fitBtn)
                "onion" -> when editing (Gtk.toggleButtonSetActive onionBtn (not s.onion))
                "sidebar" -> Gtk.toggleButtonGetActive chatBtn >>= Gtk.toggleButtonSetActive chatBtn . not
                "assistant" -> Gtk.toggleButtonSetActive chatBtn True >> Gtk.stackSetVisibleChildName side "assistant"
                "references" -> Gtk.toggleButtonSetActive chatBtn True >> Gtk.stackSetVisibleChildName side "board"
                "play" -> when editing play
                "prev-frame" -> when editing (stop >> step (-1))
                "next-frame" -> when editing (stop >> step 1)
                "tool-select" -> tool selectR
                "tool-brush" -> tool brushR
                "tool-scatter" -> tool scatterR
                "tool-eraser" -> tool eraserR
                "tool-shapes" -> tool shapeR
                "tool-fill" -> tool fillR
                "tool-text" -> tool textR
                "tool-camera" -> tool cameraR
                _ -> pure ()
    pure Window {window = win, canvasArea = canvas, connectTo = connect, openId = openProject, inspect = probe, say = say, pointer = onPointer, key = onKey, keyUp = onKeyUp, click = clickNamed, addReferenceFile = addReferenceFile, connectAssistant = useModel, tellAssistant = sendChat, exportFile = exportTo, openLayerPanel = openPanel, setInPanel = panelSet, setShotWith = shotSet, command = runCommand}

-- | The hold of drawing `n` (from 1) of layer `li`.
holdOf :: Int -> Int -> Project -> Either Text Int
holdOf li n p = case lookup li (zip [0 ..] (viewProject p).stack) of
    Just LayerView {kind = DrawingView _ cels} | Just c <- lookup (n - 1) (zip [0 ..] cels) -> Right c.hold
    _ -> Left "expected a drawing"

-- | The items of drawing `n` (from 1) of layer `li`, as drawn.
celItems :: ProjectView -> Int -> Int -> [ItemView]
celItems v li n = case lookup li (zip [0 ..] v.stack) of
    Just LayerView {kind = DrawingView _ cels} | Just c <- lookup (n - 1) (zip [0 ..] cels) -> c.items
    _ -> []

middle :: Box -> (Double, Double)
middle b = ((b.x0 + b.x1) / 2, (b.y0 + b.y1) / 2)

sortDesc :: [Int] -> [Int]
sortDesc = foldr insertDesc []
    where
        insertDesc x [] = [x]
        insertDesc x (y : ys) = if x >= y then x : y : ys else y : insertDesc x ys

isEraser :: ItemView -> Bool
isEraser = \case
    EraseV _ -> True
    _ -> False

-- Opening ---------------------------------------------------------------------------------

opened :: Backend -> ProjectId -> Project -> Revision -> Maybe Pictures -> IO Open
opened b pid p rev known = do
    let
        v = viewProject p
    pics <- maybe (loadPictures b v) pure known
    cache <- newCache
    pure Open {pid, project = p, view = v, atRevision = rev, env = Env {pictures = pics, cache, project = v}}

-- | The same open project at a new version: its view and drawings drawn afresh.
reopen :: Open -> Project -> IO Open
reopen o p = do
    let
        v = viewProject p
    cache <- newCache
    pure o {project = p, view = v, env = o.env {cache, project = v}}

-- | Every picture the drawings use, as Cairo surfaces (GdkPixbuf reads PNG, JPEG, WebP…).
loadPictures :: Backend -> ProjectView -> IO Pictures
loadPictures b v = do
    let
        assets =
            [i.asset | LayerView {kind = DrawingView _ cels} <- v.stack, c <- cels, ImageV i <- c.items]
                <> [a | LayerView {kind = ReferenceView a Image} <- v.stack]
                <> [r.asset | r <- v.board, r.media == Image]
    found <- forM (Map.keys (Map.fromList [(a, ()) | a <- assets])) $ \a -> do
        path <- b.picture a
        case path of
            Nothing -> pure Nothing
            Just file -> do
                r <- try (Pixbuf.pixbufNewFromFile file) :: IO (Either SomeException (Maybe Pixbuf.Pixbuf))
                case r of
                    Right (Just pb) -> do
                        w <- Pixbuf.pixbufGetWidth pb
                        h <- Pixbuf.pixbufGetHeight pb
                        s <- C.createImageSurface C.FormatARGB32 (fromIntegral w) (fromIntegral h)
                        C.renderWith s $ do
                            ctx <- Connector.getContext
                            C.liftIO (Gdk.cairoSetSourcePixbuf ctx pb 0 0)
                            C.paint
                        pure (Just (a, s))
                    _ -> pure Nothing
    pure (Map.fromList (catMaybes found))

topDrawing :: ProjectView -> Int
topDrawing v = case [i | (i, LayerView {kind = DrawingView _ _}) <- zip [0 ..] v.stack] of
    [] -> 0
    is -> last is

-- | A new project of one blank drawing, 1920 × 1080 at 24 fps, saved before it opens.
createProject :: Backend -> Text -> IO (Either Text ProjectId)
createProject b name = do
    raw <- newUuid
    case parse raw of
        Left why -> pure (Left why)
        Right pid -> case (parse name, canvasFrom (Aeson.object ["width" Aeson..= (1920 :: Int), "height" Aeson..= (1080 :: Int)]), fpsFrom (Aeson.Number 24)) of
            (Right pname, Right c, Right f) ->
                b.save Nothing (newProject pid pname c f) >>= \case
                    Saved _ -> pure (Right pid)
                    Stale _ -> pure (Left "a project with that id exists")
                    Failed why -> pure (Left why)
            (Left why, _, _) -> pure (Left why)
            _ -> pure (Left "a new project could not be made")

-- | The project under a new id and "(copy)" after its name, parsed like any other.
copyOf :: Project -> IO (Maybe Project)
copyOf p = do
    raw <- newUuid
    pure $ case unparse p of
        Aeson.Object o ->
            let
                name = case KeyMap.lookup "name" o of
                    Just (Aeson.String n) -> Text.take 70 n <> " (copy)"
                    _ -> "Copy"
            in
                either (const Nothing) Just (parse (Aeson.Object (KeyMap.insert "id" (Aeson.String raw) (KeyMap.insert "name" (Aeson.String name) o))))
        _ -> Nothing

-- Drawing ---------------------------------------------------------------------------------

headerW, rulerH, rowH, unitW :: Double
headerW = 160
rulerH = 24
rowH = 32
unitW = 16

-- | Widget pixels → canvas pixels.
canvasPoint :: State -> (Double, Double) -> (Double, Double)
canvasPoint s (x, y) = ((x - s.panX) / s.magnify, (y - s.panY) / s.magnify)

drawCanvas :: State -> [(Double, C.Surface)] -> (Draft -> Maybe (Either Text Item)) -> C.Render ()
drawCanvas s under finishing = do
    rgb workspace
    C.paint
    forM_ s.open $ \o -> do
        let
            v = o.view
            pw = fromIntegral v.width
            ph = fromIntegral v.height
        -- A soft shadow under the paper, in screen pixels.
        forM_ [(12, 0.3), (6, 0.5), (2, 0.9)] $ \(spread, a) -> do
            rgba (paperShadow, a)
            rounded (s.panX - spread) (s.panY - spread + spread / 2) (pw * s.magnify + 2 * spread) (ph * s.magnify + 2 * spread) (spread + 2)
            C.fill
        C.translate s.panX s.panY
        C.scale s.magnify s.magnify
        C.setSourceRGB 1 1 1
        C.rectangle 0 0 pw ph
        C.fill
        -- Reference layers, under the drawings, fitted to the paper as the browser fits them.
        forM_ under (uncurry (drawFitted pw ph))
        -- Onion skin: the drawings before and after this one on the chosen layer, tinted.
        when (s.onion && isNothing s.playing) $ forM_ (lookup s.layer (zip [0 ..] v.stack)) $ \l -> case l.kind of
            DrawingView holdLast cels -> forM_ (celAt holdLast cels s.playhead) $ \ci ->
                forM_ [(ci - 1, (0.9, 0.28, 0.3)), (ci + 1, (0.19, 0.64, 0.42))] $ \(k, tint) ->
                    forM_ (lookup k (zip [0 ..] cels)) $ \cel -> do
                        C.pushGroup
                        drawCel o.env.pictures (Just tint) cel.items
                        C.popGroupToSource
                        C.paintWithAlpha 0.35
            _ -> pure ()
        let
            selected n = [it | (i, it) <- zip [0 :: Int ..] (celItems v s.layer n), i `elem` s.selection]
            here = case lookup s.layer (zip [0 ..] v.stack) of
                Just LayerView {kind = DrawingView holdLast cels} -> fmap (+ 1) (celAt holdLast cels s.playhead)
                _ -> Nothing
            -- While a selection is dragged, the edited drawing shows it where it would go.
            preview = do
                sd <- s.selDrag
                n <- here
                let
                    items = celItems v s.layer n
                    rest = [it | (i, it) <- zip [0 :: Int ..] items, i `notElem` s.selection]
                pure
                    ( s.layer
                    , forM_ [ColorPlane, LinePlane] $ \pl -> do
                        C.pushGroup
                        drawItems o.env.pictures Nothing pl rest
                        C.save
                        C.transform sd.current
                        drawItems o.env.pictures Nothing pl (selected n)
                        C.restore
                        C.popGroupToSource
                        C.paint
                    )
        drawLayers o.env s.playhead [i | (i, l) <- zip [0 ..] v.stack, not (isReference l)] Nothing preview
        -- The camera tool: the shot as a frame over the paper, the rest dimmed.
        when (s.tool == CameraTool) $ do
            let
                (ccx, ccy, cz, cr) = case cameraAt v s.playhead of
                    Just c -> (c.cx, c.cy, c.zoom, c.rotation)
                    Nothing -> (pw / 2, ph / 2, 1, 0)
                fw = pw / cz
                fh = ph / cz
            C.save
            C.setFillRule C.FillRuleEvenOdd
            C.rectangle (-(pw * 4)) (-(ph * 4)) (pw * 9) (ph * 9)
            C.translate ccx ccy
            C.rotate cr
            C.rectangle (-(fw / 2)) (-(fh / 2)) fw fh
            C.setSourceRGBA 0 0 0 0.38
            C.fill
            C.rectangle (-(fw / 2)) (-(fh / 2)) fw fh
            rgb accent
            C.setLineWidth (2 / s.magnify)
            C.stroke
            forM_ [(-1, 0), (1, 0), (0, -1), (0, 1)] $ \(dx, dy) -> do
                C.moveTo (dx * 4 / s.magnify) (dy * 4 / s.magnify)
                C.lineTo (dx * 14 / s.magnify) (dy * 14 / s.magnify)
            C.stroke
            C.restore
        -- The selection's box, its corner handles (scale) and the round handle above (turn).
        forM_ (here >>= \n -> boxOf (selected n)) $ \bx -> forM_ (lookup s.layer (zip [0 ..] v.stack)) $ \l -> do
            let
                (m, _) = placement l s.playhead
                px = 1 / s.magnify
            C.save
            C.transform m
            forM_ s.selDrag (C.transform . (.current))
            rgb accent
            C.setLineWidth (1.5 * px)
            C.rectangle bx.x0 bx.y0 (bx.x1 - bx.x0) (bx.y1 - bx.y0)
            C.stroke
            forM_ [(bx.x0, bx.y0), (bx.x1, bx.y0), (bx.x1, bx.y1), (bx.x0, bx.y1)] $ \(hx, hy) -> do
                C.rectangle (hx - 5 * px) (hy - 5 * px) (10 * px) (10 * px)
                C.fill
            C.arc ((bx.x0 + bx.x1) / 2) (bx.y0 - 30 * px) (6 * px) 0 (2 * pi)
            C.fill
            C.restore
        -- The stroke or shape in progress, on the edited layer's place.
        forM_ s.draft $ \d -> forM_ (finishing d) $ \case
            Right made -> forM_ (lookup s.layer (zip [0 ..] v.stack)) $ \l -> do
                let
                    (m, _) = placement l s.playhead
                    item = viewItem made
                C.save
                C.transform m
                drawItem o.env.pictures (if isErase item then Just (0.55, 0.55, 0.6) else Nothing) item
                C.restore
            Left _ -> pure ()
    where
        isReference l = case l.kind of
            ReferenceView _ _ -> True
            _ -> False
        isErase = \case
            EraseV _ -> True
            _ -> False

drawTimeline :: State -> Double -> Double -> C.Render ()
drawTimeline s w h = do
    rgb ground
    C.paint
    forM_ s.open $ \o -> do
        let
            v = o.view
            total = duration v
            x0 = headerW - s.tlScroll
            frameX f = x0 + fromIntegral f * unitW
            layers = reverse (zip [0 :: Int ..] v.stack)
            visibleX x = x >= headerW - unitW && x <= w
        -- Ruler: a tick a frame, the frame number every fifth.
        rgb surface
        C.rectangle 0 0 w rulerH
        C.fill
        forM_ [0 .. total + 48] $ \f -> do
            let
                x = frameX f
                labelled = (f + 1) `mod` 5 == 0 || f == 0
            when (visibleX x && x >= headerW) $ do
                rgba (dimText, if labelled then 0.7 else 0.3)
                C.rectangle (x + unitW / 2) (rulerH - (if labelled then 7 else 4)) 1 (if labelled then 7 else 4)
                C.fill
                when labelled $ do
                    rgb dimText
                    C.moveTo (x + unitW / 2 + 3) 3
                    void (uiText 10 False (Text.pack (show (f + 1))))
        rgba hairline
        C.rectangle 0 (rulerH - 1) w 1
        C.fill
        -- Camera keys: diamonds on the ruler.
        forM_ v.shots $ \k -> do
            let
                kx = frameX k.frame + unitW / 2
            when (kx >= headerW && kx <= w) $ do
                C.setSourceRGB (0xf3 / 255) (0xc8 / 255) (0x43 / 255)
                C.moveTo kx (rulerH - 10)
                C.lineTo (kx + 4) (rulerH - 6)
                C.lineTo kx (rulerH - 2)
                C.lineTo (kx - 4) (rulerH - 6)
                C.closePath
                C.fill
        -- Rows: each drawing a block as wide as it is held.
        forM_ (zip [0 :: Int ..] layers) $ \(row, (li, l)) -> do
            let
                y = rulerH + fromIntegral row * rowH
            when (li == s.layer) $ do
                rgba (accent, 0.07)
                C.rectangle headerW y (w - headerW) rowH
                C.fill
            case l.kind of
                DrawingView holdLast cels -> do
                    let
                        here = celAt holdLast cels s.playhead
                        starts = scanl (+) 0 (map (.hold) cels)
                    forM_ (zip3 [0 :: Int ..] cels starts) $ \(ci, cel, start) -> do
                        let
                            x = frameX start + 1
                            cw = fromIntegral cel.hold * unitW - 2
                        unless (x + cw < headerW || x > w) $ do
                            rounded x (y + 7) cw (rowH - 14) 3
                            if li == s.layer && here == Just ci
                                then rgb accent >> C.fill
                                else
                                    if null cel.items
                                        then rgb celEmpty >> C.setLineWidth 1 >> C.stroke
                                        else rgb celFill >> C.fill
                LightLayerView _ -> band y (1, 0.85, 0.5)
                ReferenceView _ _ -> band y (0.6, 0.6, 0.65)
            rgba hairline
            C.rectangle 0 (y + rowH - 1) w 1
            C.fill
            -- The layer's name, in a column the lanes scroll under.
            rgb (if li == s.layer then raised else surface)
            C.rectangle 0 y headerW (rowH - 1)
            C.fill
            when (li == s.layer) $ do
                rgb accent
                C.rectangle 0 y 2 (rowH - 1)
                C.fill
            rgba (textColour, if l.visible then 1 else 0.4)
            C.moveTo 12 (y + rowH / 2 - 8)
            void (uiText 12 (li == s.layer) (Text.take 18 l.name))
            when l.locked $ do
                rgb dimText
                C.moveTo (headerW - 48) (y + rowH / 2 - 7)
                void (uiText 10 False "locked")
        rgba hairline
        C.rectangle (headerW - 1) 0 1 h
        C.fill
        -- The playhead: a line down the lanes and its frame number on the ruler.
        let
            px = frameX s.playhead + unitW / 2
        when (px >= headerW) $ do
            rgb accent
            C.rectangle (px - 0.75) rulerH 1.5 (h - rulerH)
            C.fill
            rounded (px - 13) 2 26 (rulerH - 5) 4
            C.fill
            C.setSourceRGB 1 1 1
            C.moveTo (px - (if s.playhead + 1 >= 10 then 7 else 3.5)) 3.5
            void (uiText 10 True (Text.pack (show (s.playhead + 1))))
    where
        band y (r, g, b) = do
            C.setSourceRGBA r g b 0.2
            rounded headerW (y + 7) (w - headerW) (rowH - 14) 3
            C.fill

-- Widgets ---------------------------------------------------------------------------------

toW :: (Gtk.IsWidget w) => w -> IO Gtk.Widget
toW = Gtk.toWidget

pack :: Gtk.Box -> [IO Gtk.Widget] -> IO ()
pack box ws = sequence ws >>= mapM_ (\w -> Gtk.boxPackStart box w False False 0)

addClass :: (Gtk.IsWidget w) => w -> Text -> IO ()
addClass w c = Gtk.widgetGetStyleContext w >>= \sc -> Gtk.styleContextAddClass sc c

separator :: IO Gtk.Widget
separator = do
    sep <- Gtk.boxNew Gtk.OrientationHorizontal 0
    addClass sep "sep"
    Gtk.widgetSetValign sep Gtk.AlignCenter
    toW sep

-- | Puts icon `name` on a button, with `tip` as its tooltip.
iconInto :: (Gtk.IsButton b, Gtk.IsWidget b) => b -> Text -> Text -> IO ()
iconInto b name tip = do
    img <- iconImage name 16
    Gtk.buttonSetImage b (Just img)
    Gtk.buttonSetAlwaysShowImage b True
    Gtk.widgetSetTooltipText b (Just tip)
    addClass b "icon"

iconButton :: Text -> Text -> IO Gtk.Button
iconButton name tip = do
    b <- Gtk.buttonNew
    iconInto b name tip
    pure b

-- | A tool in the rail: one of a group, shown as its icon.
toolButton :: Maybe Gtk.RadioButton -> Text -> Text -> IO Gtk.RadioButton
toolButton group name tip = do
    r <- Gtk.radioButtonNewFromWidget group
    Gtk.toggleButtonSetMode r False
    img <- iconImage name 18
    Gtk.buttonSetImage r (Just img)
    Gtk.buttonSetAlwaysShowImage r True
    Gtk.widgetSetTooltipText r (Just tip)
    pure r

{- | A row of choices, one at a time, each an icon (Right) or a word (Left); `chose` hears the
value picked. Also returns a way to pick one from code.
-}
segmented :: [(Text, Either Text Text, Text)] -> (Text -> IO ()) -> IO (Gtk.Box, Text -> IO ())
segmented choices chose = do
    box <- Gtk.boxNew Gtk.OrientationHorizontal 0
    addClass box "segmented"
    Gtk.widgetSetValign box Gtk.AlignCenter
    buttons <- go Nothing choices
    forM_ buttons $ \(_, r) -> Gtk.boxPackStart box r False False 0
    pure (box, \v -> forM_ (lookup v buttons) (`Gtk.toggleButtonSetActive` True))
    where
        go _ [] = pure []
        go group ((value, face, tip) : rest) = do
            r <- Gtk.radioButtonNewFromWidget group
            Gtk.toggleButtonSetMode r False
            case face of
                Right name -> iconImage name 16 >>= \img -> Gtk.buttonSetImage r (Just img) >> Gtk.buttonSetAlwaysShowImage r True
                Left word -> Gtk.buttonSetLabel r word
            Gtk.widgetSetTooltipText r (Just tip)
            _ <- on r #toggled (Gtk.toggleButtonGetActive r >>= \on' -> when on' (chose value))
            more <- go (Just (fromMaybe r group)) rest
            pure ((value, r) : more)

-- | A project on the list: its first frame, its name, and its size and length.
projectCard :: Backend -> ProjectSummary -> (ProjectId -> IO ()) -> IO Gtk.Button
projectCard b p openIt = do
    btn <- Gtk.buttonNew
    addClass btn "card"
    box <- Gtk.boxNew Gtk.OrientationVertical 0
    thumb <- new Gtk.DrawingArea []
    Gtk.widgetSetSizeRequest thumb 232 130
    envRef <- newIORef Nothing
    _ <- on thumb #draw $ \ctx -> do
        e <- readIORef envRef
        tw <- Gtk.widgetGetAllocatedWidth thumb
        th <- Gtk.widgetGetAllocatedHeight thumb
        Connector.renderWithContext (thumbnail e (fromIntegral tw) (fromIntegral th)) ctx
        pure True
    name <- Gtk.labelNew (Just (unparse (summaryName p)))
    addClass name "name"
    meta <- Gtk.labelNew (Just " ")
    addClass meta "meta"
    forM_ [name, meta] $ \l -> do
        Gtk.labelSetXalign l 0
        Gtk.labelSetEllipsize l Pango.EllipsizeModeEnd
        Gtk.widgetSetMarginStart l 12
        Gtk.widgetSetMarginEnd l 12
    Gtk.widgetSetMarginTop name 10
    Gtk.widgetSetMarginBottom meta 12
    Gtk.boxPackStart box thumb False False 0
    Gtk.boxPackStart box name False False 0
    Gtk.boxPackStart box meta False False 2
    Gtk.containerAdd btn box
    _ <- on btn #clicked (openIt (summaryId p))
    background (b.load (summaryId p)) $ \case
        Right (pr, _) -> do
            let
                v = viewProject pr
            pics <- loadPictures b v
            cache <- newCache
            writeIORef envRef (Just Env {pictures = pics, cache, project = v})
            Gtk.labelSetText meta (Text.pack (show v.width <> " × " <> show v.height <> " · " <> show v.rate <> " fps · " <> show (duration v) <> (if duration v == 1 then " frame" else " frames")))
            Gtk.widgetQueueDraw thumb
        Left _ -> pure ()
    pure btn

{- | Which model the assistant talks to: the provider, its address, the model and the key
(left empty to keep the saved one).
-}
connectDialog :: Gtk.Window -> Maybe Settings -> IO (Maybe (Settings, Maybe ApiKey))
connectDialog parent current = do
    d <- new Gtk.Dialog [#title := "Connect a model", #modal := True]
    Gtk.windowSetTransientFor d (Just parent)
    area <- Gtk.dialogGetContentArea d
    grid <- new Gtk.Grid [#rowSpacing := 10, #columnSpacing := 12, #margin := 18]
    providerC <- Gtk.comboBoxTextNew
    Gtk.comboBoxTextAppend providerC (Just "anthropic") "Anthropic (Claude)"
    Gtk.comboBoxTextAppend providerC (Just "openai") "OpenAI-compatible (OpenAI, Ollama, LM Studio…)"
    urlE <- Gtk.entryNew
    modelE <- Gtk.entryNew
    keyE <- Gtk.entryNew
    Gtk.entrySetVisibility keyE False
    Gtk.entrySetPlaceholderText keyE (Just "Leave empty to keep the saved key")
    note <- Gtk.labelNew (Just "The key is kept in this computer's keychain, never in a file of the project.")
    addClass note "dim"
    Gtk.labelSetLineWrap note True
    Gtk.labelSetMaxWidthChars note 46
    Gtk.labelSetXalign note 0
    forM_ (zip [0 ..] [("Provider", toW providerC), ("Address", toW urlE), ("Model", toW modelE), ("API key", toW keyE)]) $ \(row, (lbl, mk)) -> do
        l <- Gtk.labelNew (Just lbl)
        Gtk.labelSetXalign l 1
        w <- mk
        Gtk.widgetSetHexpand w True
        Gtk.gridAttach grid l 0 row 1 1
        Gtk.gridAttach grid w 1 row 1 1
    Gtk.gridAttach grid note 0 4 2 1
    Gtk.containerAdd area grid
    let
        fill cfg = do
            Gtk.entrySetText urlE cfg.url
            Gtk.entrySetText modelE cfg.model
    case current of
        Just cfg -> Gtk.comboBoxSetActiveId providerC (Just (providerName cfg.provider)) >> fill cfg
        Nothing -> Gtk.comboBoxSetActiveId providerC (Just "anthropic") >> fill (defaultSettings Anthropic)
    _ <- on providerC #changed $ do
        pid <- Gtk.comboBoxGetActiveId providerC
        fill (defaultSettings (if pid == Just "openai" then OpenAICompatible else Anthropic))
    Gtk.widgetShowAll d
    let
        ask = do
            okd <- dialogRun d
            if not okd
                then pure Nothing
                else do
                    pid <- Gtk.comboBoxGetActiveId providerC
                    u <- Gtk.entryGetText urlE
                    m <- Gtk.entryGetText modelE
                    k <- Gtk.entryGetText keyE
                    let
                        cfg = parseSettings (Aeson.object ["provider" Aeson..= fromMaybe "anthropic" pid, "url" Aeson..= u, "model" Aeson..= m])
                        key = if Text.null (Text.strip k) then Right Nothing else Just <$> parseApiKey k
                    case (,) <$> cfg <*> key of
                        Right r -> pure (Just r)
                        Left why -> do
                            Gtk.labelSetText note why
                            ask
    r <- ask
    Gtk.widgetDestroy d
    pure r

-- | What to export: size, format, quality, fit, and a transparent background where kept.
exportDialog :: Gtk.Window -> ProjectView -> IO (Maybe Export)
exportDialog parent v = do
    d <- new Gtk.Dialog [#title := "Export", #modal := True]
    Gtk.windowSetTransientFor d (Just parent)
    area <- Gtk.dialogGetContentArea d
    grid <- new Gtk.Grid [#rowSpacing := 10, #columnSpacing := 12, #margin := 18]
    let
        sizes = presets v.width v.height
        formats = [minBound .. maxBound] :: [Format]
        qualities = [(Standard, "Standard (YouTube's rate)"), (High, "High (1.5×)"), (Max, "Max (2.5×)")]
    sizeC <- Gtk.comboBoxTextNew
    forM_ sizes $ \z -> Gtk.comboBoxTextAppendText sizeC z.sizeName
    Gtk.comboBoxSetActive sizeC 0
    formatC <- Gtk.comboBoxTextNew
    forM_ formats $ \f -> Gtk.comboBoxTextAppendText formatC (formatName f)
    Gtk.comboBoxSetActive formatC 0
    qualityC <- Gtk.comboBoxTextNew
    forM_ qualities $ \(_, n) -> Gtk.comboBoxTextAppendText qualityC n
    Gtk.comboBoxSetActive qualityC 1
    fitC <- Gtk.comboBoxTextNew
    Gtk.comboBoxTextAppendText fitC "Fit inside (letterbox)"
    Gtk.comboBoxTextAppendText fitC "Fill (crop)"
    Gtk.comboBoxSetActive fitC 0
    clearB <- Gtk.checkButtonNewWithLabel "Transparent background"
    note <- Gtk.labelNew (Just (Text.pack (show (duration v) <> " frames at " <> show v.rate <> " fps, through the camera; reference layers are left out.")))
    addClass note "dim"
    Gtk.labelSetXalign note 0
    Gtk.labelSetLineWrap note True
    forM_ (zip [0 ..] [("Size", toW sizeC), ("Format", toW formatC), ("Quality", toW qualityC), ("Shape", toW fitC)]) $ \(row, (lbl, mk)) -> do
        l <- Gtk.labelNew (Just lbl)
        Gtk.labelSetXalign l 1
        w <- mk
        Gtk.widgetSetHexpand w True
        Gtk.gridAttach grid l 0 row 1 1
        Gtk.gridAttach grid w 1 row 1 1
    Gtk.gridAttach grid clearB 1 4 1 1
    Gtk.gridAttach grid note 0 5 2 1
    Gtk.containerAdd area grid
    let
        pick :: [a] -> Gtk.ComboBoxText -> IO (Maybe a)
        pick xs c = Gtk.comboBoxGetActive c >>= \i -> pure (lookup (fromIntegral i) (zip [0 :: Int ..] xs))
        sync = do
            f <- fromMaybe H264 <$> pick formats formatC
            Gtk.widgetSetSensitive clearB (keepsAlpha f)
            Gtk.widgetSetSensitive qualityC (f `elem` [H264, Hevc])
    _ <- on formatC #changed sync
    sync
    _ <- on sizeC #changed $ do
        -- A different shape than the canvas: fitting matters.
        z <- pick sizes sizeC
        Gtk.widgetSetSensitive fitC (maybe False (\x -> x.width * v.height /= x.height * v.width) z)
    Gtk.widgetSetSensitive fitC False
    Gtk.widgetShowAll d
    okd <- dialogRun d
    r <-
        if not okd
            then pure Nothing
            else do
                z <- pick sizes sizeC
                f <- pick formats formatC
                q <- pick (map fst qualities) qualityC
                fitI <- Gtk.comboBoxGetActive fitC
                clear <- Gtk.toggleButtonGetActive clearB
                pure $ (\z' f' q' -> Export {format = f', size = z', quality = q', fit = if fitI == 1 then Cover else Contain, transparent = clear}) <$> z <*> f <*> q
    Gtk.widgetDestroy d
    pure r

-- | Where the film goes: a file named after the project (a folder for PNG frames).
saveAs :: Gtk.Window -> Text -> Format -> IO (Maybe FilePath)
saveAs parent title f = do
    let
        folder = f == PngFrames
    d <- new Gtk.FileChooserDialog [#title := (if folder then "Export the frames into" else "Export as"), #action := (if folder then Gtk.FileChooserActionSelectFolder else Gtk.FileChooserActionSave), #modal := True, #doOverwriteConfirmation := True]
    Gtk.windowSetTransientFor d (Just parent)
    _ <- Gtk.dialogAddButton d "Cancel" (fromIntegral (fromEnum Gtk.ResponseTypeCancel))
    _ <- Gtk.dialogAddButton d "Export" (fromIntegral (fromEnum Gtk.ResponseTypeAccept))
    unless folder $ Gtk.fileChooserSetCurrentName d (Text.filter (`notElem` ("/\\:" :: String)) title <> "." <> formatExtension f)
    answer <- Gtk.dialogRun d
    path <- if answer == fromIntegral (fromEnum Gtk.ResponseTypeAccept) then Gtk.fileChooserGetFilename d else pure Nothing
    Gtk.widgetDestroy d
    pure path

-- | A camera key as the project's JSON has it.
shotValue :: Shot -> Aeson.Value
shotValue k =
    Aeson.object
        [ "frame" Aeson..= k.atFrame
        , "center" Aeson..= [fst k.shotCentre, snd k.shotCentre]
        , "zoom" Aeson..= max 0.1 (min 10 k.shotZoom)
        , "rotation" Aeson..= k.shotTurn
        , "ease" Aeson..= easeId k.shotEase
        , "shake" Aeson..= k.shotShake
        , "focus" Aeson..= max 0.2 (min 20 k.shotFocus)
        , "aperture" Aeson..= k.shotBlur
        , "tint" Aeson..= k.shotTint
        , "tintAmount" Aeson..= k.shotTintAmount
        ]

easeId :: Ease -> Text
easeId = \case
    Linear -> "linear"
    EaseIn -> "in"
    EaseOut -> "out"
    EaseInOut -> "inOut"

easeFromId :: Maybe Text -> Ease
easeFromId = \case
    Just "linear" -> Linear
    Just "in" -> EaseIn
    Just "out" -> EaseOut
    _ -> EaseInOut

-- | A colour as #rrggbb, from channels 0 to 1.
rgbHex :: (Double, Double, Double) -> Text
rgbHex (r, g, b) = Text.pack ('#' : concatMap byte [r, g, b])
    where
        byte v = let n = max 0 (min 255 (round (v * 255) :: Int)) in (if n < 16 then "0" else "") <> showHex n ""

blendName :: Blend -> Text
blendName = Text.toLower . Text.pack . show

-- | Asks for a picture or video file to put on the board.
chooseFile :: Gtk.Window -> IO (Maybe FilePath)
chooseFile parent = do
    d <- new Gtk.FileChooserDialog [#title := "Add a reference", #action := Gtk.FileChooserActionOpen, #modal := True]
    Gtk.windowSetTransientFor d (Just parent)
    _ <- Gtk.dialogAddButton d "Cancel" (fromIntegral (fromEnum Gtk.ResponseTypeCancel))
    _ <- Gtk.dialogAddButton d "Add" (fromIntegral (fromEnum Gtk.ResponseTypeAccept))
    only <- Gtk.fileFilterNew
    Gtk.fileFilterSetName only (Just "Pictures and videos")
    forM_ ["*.png", "*.jpg", "*.jpeg", "*.gif", "*.webp", "*.mp4", "*.m4v", "*.mov", "*.webm"] (Gtk.fileFilterAddPattern only)
    Gtk.fileChooserAddFilter d only
    answer <- Gtk.dialogRun d
    path <- if answer == fromIntegral (fromEnum Gtk.ResponseTypeAccept) then Gtk.fileChooserGetFilename d else pure Nothing
    Gtk.widgetDestroy d
    pure path

-- | A picture fitted into w × h, centred, at `alpha`.
drawFitted :: Double -> Double -> Double -> C.Surface -> C.Render ()
drawFitted w h alpha surf = do
    sw <- fromIntegral <$> C.imageSurfaceGetWidth surf
    sh <- fromIntegral <$> C.imageSurfaceGetHeight surf
    when (sw > 0 && sh > 0) $ do
        let
            k = min (w / sw) (h / sh)
        C.save
        C.translate ((w - sw * k) / 2) ((h - sh * k) / 2)
        C.scale k k
        C.setSourceSurface surf 0 0
        C.paintWithAlpha alpha
        C.restore

-- | Where a reference lands in the board's view: fitted, then zoomed by k and moved by (x, y).
boardPlace :: Double -> Double -> (Double, Double, Double) -> (Double, Double) -> (Double, Double, Double)
boardPlace w h (vk, vx, vy) (sw, sh) =
    let
        k = min (w / sw) (h / sh) * vk
    in
        ((w - sw * k) / 2 + vx, (h - sh * k) / 2 + vy, k)

-- | The colour of a picture's pixel as #rrggbb, unless it is see-through.
pixelAt :: C.Surface -> Int -> Int -> IO (Maybe Text)
pixelAt surf px py = do
    w <- C.imageSurfaceGetWidth surf
    h <- C.imageSurfaceGetHeight surf
    if px < 0 || py < 0 || px >= w || py >= h
        then pure Nothing
        else do
            C.surfaceFlush surf
            stride <- C.imageSurfaceGetStride surf
            bytes <- C.imageSurfaceGetData surf
            let
                at k = fromIntegral (BS.index bytes (py * stride + px * 4 + k)) :: Int
                a = at 3
                un c = min 255 (if a == 0 then 0 else c * 255 `div` a)
                hex c = let t = showHex c "" in if length t < 2 then '0' : t else t
            pure (if a < 8 then Nothing else Just (Text.pack ('#' : concatMap (hex . un) [at 2, at 1, at 0])))

-- | A project's first frame, fitted into w × h with rounded top corners.
thumbnail :: Maybe Env -> Double -> Double -> C.Render ()
thumbnail e w h = do
    rounded 1 1 (w - 2) (h + 20) 8
    C.clip
    rgb workspace
    C.paint
    forM_ e $ \env -> do
        let
            v = env.project
            k = min (w / fromIntegral v.width) (h / fromIntegral v.height)
            dw = fromIntegral v.width * k
            dh = fromIntegral v.height * k
        C.translate ((w - dw) / 2) ((h - dh) / 2)
        drawFrame env 0 (max 1 (round dw)) (max 1 (round dh)) True True

setColour :: Gtk.ColorButton -> Text -> IO ()
setColour b hex = do
    c <- new Gdk.RGBA []
    ok <- Gdk.rGBAParse c hex
    when ok (Gtk.colorChooserSetRgba b c)

-- | The chosen colour as #rrggbb.
getColour :: Gtk.ColorButton -> IO Text
getColour b = do
    c <- Gtk.colorChooserGetRgba b
    r <- Gdk.getRGBARed c
    g <- Gdk.getRGBAGreen c
    bl <- Gdk.getRGBABlue c
    let
        byte v = let n = max 0 (min 255 (round (v * 255) :: Int)) in (if n < 16 then "0" else "") <> showHex n ""
    pure (Text.pack ("#" <> byte r <> byte g <> byte bl))

dialogRun :: Gtk.Dialog -> IO Bool
dialogRun d = do
    _ <- Gtk.dialogAddButton d "Cancel" (fromIntegral (fromEnum Gtk.ResponseTypeCancel))
    _ <- Gtk.dialogAddButton d "OK" (fromIntegral (fromEnum Gtk.ResponseTypeOk))
    Gtk.dialogSetDefaultResponse d (fromIntegral (fromEnum Gtk.ResponseTypeOk))
    Gtk.widgetShowAll d
    r <- Gtk.dialogRun d
    pure (r == (fromIntegral (fromEnum Gtk.ResponseTypeOk) :: Int32))

-- | Where projects live: a folder on this computer, or a server (address, and header lines).
connectionDialog :: Gtk.Window -> Maybe Connection -> IO (Maybe Connection)
connectionDialog parent current = do
    d <- new Gtk.Dialog [#title := "Where projects live", #modal := True, #transientFor := parent, #defaultWidth := 560]
    area <- Gtk.dialogGetContentArea d
    folder0 <- defaultFolder
    local <- Gtk.radioButtonNewWithLabel ([] :: [Gtk.RadioButton]) "On this computer, in this folder:"
    folderEntry <- Gtk.entryNew
    Gtk.entrySetText folderEntry (Text.pack (case current of Just (Local f) -> f; _ -> folder0))
    remote <- Gtk.radioButtonNewWithLabelFromWidget (Just local) "On my anima2D server, at this address:"
    urlEntry <- Gtk.entryNew
    Gtk.entrySetPlaceholderText urlEntry (Just "https://anim.example.com")
    -- Keycloak (or any OpenID Connect provider) in front of the server.
    ssoB <- Gtk.checkButtonNewWithLabel "Sign in with Keycloak (OpenID Connect)"
    issuerEntry <- Gtk.entryNew
    Gtk.entrySetPlaceholderText issuerEntry (Just "https://auth.example.com/realms/anima")
    clientEntry <- Gtk.entryNew
    Gtk.entrySetText clientEntry "anima2d-desktop"
    signInB <- Gtk.buttonNewWithLabel "Sign in…"
    addClass signInB "primary"
    signOutB <- Gtk.buttonNewWithLabel "Sign out"
    ssoState <- Gtk.labelNew (Just "")
    addClass ssoState "dim"
    Gtk.labelSetXalign ssoState 0
    Gtk.labelSetLineWrap ssoState True
    ssoNote <- Gtk.labelNew (Just ("In the realm, make a public client with standard flow and PKCE (S256), and add " <> redirectUri redirectPort <> " to its valid redirect URIs. The sign-in opens in your browser; the app keeps only the refresh token, in the keychain."))
    addClass ssoNote "dim"
    Gtk.labelSetLineWrap ssoNote True
    Gtk.labelSetMaxWidthChars ssoNote 60
    Gtk.labelSetXalign ssoNote 0
    ssoGrid <- new Gtk.Grid [#rowSpacing := 6, #columnSpacing := 10]
    issuerL <- Gtk.labelNew (Just "Realm address")
    clientL <- Gtk.labelNew (Just "Client ID")
    forM_ [issuerL, clientL] $ \l -> Gtk.labelSetXalign l 1
    Gtk.gridAttach ssoGrid issuerL 0 0 1 1
    Gtk.gridAttach ssoGrid issuerEntry 1 0 1 1
    Gtk.gridAttach ssoGrid clientL 0 1 1 1
    Gtk.gridAttach ssoGrid clientEntry 1 1 1 1
    Gtk.widgetSetHexpand issuerEntry True
    ssoButtons <- Gtk.boxNew Gtk.OrientationHorizontal 8
    Gtk.boxPackStart ssoButtons signInB False False 0
    Gtk.boxPackStart ssoButtons signOutB False False 0
    Gtk.boxPackStart ssoButtons ssoState True True 0
    hint <- Gtk.labelNew (Just "Headers to send with every request, one per line as Name: value (an access token for a sign-in gate, for example):")
    Gtk.labelSetLineWrap hint True
    headers <- Gtk.textViewNew
    Gtk.widgetSetSizeRequest headers (-1) 60
    case current of
        Just (Remote u hs sso) -> do
            Gtk.toggleButtonSetActive remote True
            Gtk.entrySetText urlEntry u
            buf <- Gtk.textViewGetBuffer headers
            Gtk.textBufferSetText buf (Text.intercalate "\n" [k <> ": " <> v | (k, v) <- hs]) (-1)
            forM_ sso $ \o -> do
                Gtk.toggleButtonSetActive ssoB True
                Gtk.entrySetText issuerEntry o.issuer
                Gtk.entrySetText clientEntry o.clientId
        _ -> Gtk.toggleButtonSetActive local True
    let
        ssoNow = do
            iss <- Gtk.entryGetText issuerEntry
            cid <- Gtk.entryGetText clientEntry
            pure (parseOidc (Aeson.object ["issuer" Aeson..= iss, "clientId" Aeson..= cid]))
        showState = do
            on' <- Gtk.toggleButtonGetActive ssoB
            forM_ [toW ssoGrid, toW ssoButtons, toW ssoNote] (>>= (`Gtk.widgetSetSensitive` on'))
            parsed <- ssoNow
            case parsed of
                Right o | on' -> do
                    kept <- loadSecret (keychainAccount o)
                    Gtk.labelSetText ssoState (if isJust kept then "Signed in" else "Not signed in")
                    Gtk.widgetSetSensitive signOutB (isJust kept)
                _ -> Gtk.labelSetText ssoState ""
    _ <- on ssoB #toggled showState
    _ <- on signInB #clicked $ do
        parsed <- ssoNow
        case parsed of
            Left why -> Gtk.labelSetText ssoState why
            Right o -> do
                Gtk.labelSetText ssoState "Waiting for the browser…"
                Gtk.widgetSetSensitive signInB False
                manager <- Tls.newTlsManager
                background (signIn manager o redirectPort (\url -> onMain (void (Gtk.showUriOnWindow (Just d) url 0)))) $ \r -> do
                    Gtk.widgetSetSensitive signInB True
                    case r of
                        Left why -> Gtk.labelSetText ssoState ("Not signed in: " <> why)
                        Right t -> do
                            kept <- maybe (pure (Left "the provider gave no refresh token (allow offline access for the client)")) (saveSecret (keychainAccount o)) t.refreshToken
                            Gtk.labelSetText ssoState $ case kept of
                                Right () -> "Signed in" <> maybe "" (" as " <>) (displayName t)
                                Left why -> "Signed in, but not kept: " <> why
                            Gtk.widgetSetSensitive signOutB True
    _ <- on signOutB #clicked $ do
        parsed <- ssoNow
        forM_ parsed $ \o -> forgetSecret (keychainAccount o)
        showState
    widgets <- sequence [toW local, toW folderEntry, toW remote, toW urlEntry, toW ssoB, toW ssoGrid, toW ssoButtons, toW ssoNote, toW hint, toW headers]
    forM_ widgets $ \wdg -> do
        Gtk.widgetSetMarginStart wdg 14
        Gtk.widgetSetMarginEnd wdg 14
        Gtk.boxPackStart area wdg False False 4
    Gtk.widgetShowAll d
    showState
    ok <- dialogRun d
    result <-
        if not ok
            then pure Nothing
            else do
                isLocal <- Gtk.toggleButtonGetActive local
                if isLocal
                    then do
                        f <- Gtk.entryGetText folderEntry
                        pure (if Text.null (Text.strip f) then Nothing else Just (Local (Text.unpack (Text.strip f))))
                    else do
                        u <- Gtk.entryGetText urlEntry
                        buf <- Gtk.textViewGetBuffer headers
                        (a, b) <- Gtk.textBufferGetBounds buf
                        txt <- Gtk.textBufferGetText buf a b False
                        useSso <- Gtk.toggleButtonGetActive ssoB
                        sso <- if useSso then either (const Nothing) Just <$> ssoNow else pure Nothing
                        let
                            hs =
                                [ (Text.strip k, Text.strip (Text.drop 1 v))
                                | ln <- Text.lines txt
                                , let
                                    (k, v) = Text.breakOn ":" ln
                                , not (Text.null (Text.strip k))
                                , not (Text.null v)
                                ]
                        pure (Just (Remote (Text.strip u) hs sso))
    Gtk.widgetDestroy d
    pure result

askText :: Gtk.Window -> Text -> Text -> IO (Maybe Text)
askText parent title initialText = do
    d <- new Gtk.Dialog [#title := title, #modal := True, #transientFor := parent]
    area <- Gtk.dialogGetContentArea d
    e <- Gtk.entryNew
    Gtk.entrySetText e initialText
    Gtk.entrySetActivatesDefault e True
    Gtk.boxPackStart area e False False 12
    ok <- dialogRun d
    t <- Gtk.entryGetText e
    Gtk.widgetDestroy d
    pure (if ok && not (Text.null (Text.strip t)) then Just (Text.strip t) else Nothing)
