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
import Anima.Types.Parse (Parse (..))
import Anima.Types.Project hiding (duration)
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, unless, void, when, zipWithM_)
import Data.GI.Base (AttrOp (..), get, new, on)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Numeric (showHex)

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Connector qualified as Connector
import GI.Cairo.Render.Matrix qualified as M
import GI.GLib qualified as GLib
import GI.Gdk qualified as Gdk
import GI.GdkPixbuf qualified as Pixbuf
import GI.Gtk qualified as Gtk

import Anima.Desktop.Backend (Backend (..), Saved (..), localBackend, remoteBackend)
import Anima.Desktop.Config (Connection (..), cacheFolder, defaultFolder, loadConnection, saveConnection)
import Anima.Desktop.Draw (Pictures, drawCel, drawItem, drawItems)
import Anima.Desktop.Geometry (Box (..), boxOf, topmostAt)
import Anima.Desktop.Make (Ink (..), ShapeTool (..))
import Anima.Desktop.Scene (Env (..), celAt, drawLayers, duration, newCache, placement)

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

data Tool = SelectTool | BrushTool | ScatterTool | EraserTool | ShapeTool | FillTool | TextTool
    deriving stock (Eq, Show)

data SelMode = Moving | Scaling | Turning
    deriving stock (Eq)

-- | A selection being dragged: how, from where, about which point, and the change so far.
data SelDrag = SelDrag {mode :: SelMode, from :: (Double, Double), centre :: (Double, Double), current :: M.Matrix}

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
    , past :: [Project]
    , future :: [Project]
    , dirty :: Bool
    , saveTimer :: Maybe GLib.Source
    , shiftHeld :: Bool
    , spaceHeld :: Bool
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
        , past = []
        , future = []
        , dirty = False
        , saveTimer = Nothing
        , shiftHeld = False
        , spaceHeld = False
        }

-- | Run on the GTK thread.
onMain :: IO () -> IO ()
onMain act = void (GLib.idleAdd GLib.PRIORITY_DEFAULT (act >> pure False))

background :: IO a -> (a -> IO ()) -> IO ()
background work done = void . forkIO $ do
    r <- work
    onMain (done r)

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
    }

runApp :: IO ()
runApp = do
    _ <- Gtk.init Nothing
    w <- buildWindow
    _ <- on w.window #destroy Gtk.mainQuit
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
        Nothing -> w.say "Choose where projects live with Connection…"
    Gtk.main

-- | Builds and shows the window; nothing is connected yet.
buildWindow :: IO Window
buildWindow = do
    -- Dark, as the canvas surround and the timeline are.
    settings <- Gtk.settingsGetDefault
    forM_ settings $ \x -> Gtk.setSettingsGtkApplicationPreferDarkTheme x True
    st <- newIORef initial
    win <- new Gtk.Window [#title := "anima2D", #defaultWidth := 1360, #defaultHeight := 880]
    header <- new Gtk.HeaderBar [#showCloseButton := True, #title := "anima2D"]
    Gtk.windowSetTitlebar win (Just header)
    backBtn <- Gtk.buttonNewWithLabel "Projects"
    connBtn <- Gtk.buttonNewWithLabel "Connection…"
    newBtn <- Gtk.buttonNewWithLabel "New project"
    undoBtn <- Gtk.buttonNewWithLabel "Undo"
    redoBtn <- Gtk.buttonNewWithLabel "Redo"
    saveState <- Gtk.labelNew (Just "")
    Gtk.headerBarPackStart header backBtn
    Gtk.headerBarPackStart header undoBtn
    Gtk.headerBarPackStart header redoBtn
    Gtk.headerBarPackStart header saveState
    Gtk.headerBarPackEnd header connBtn
    Gtk.headerBarPackEnd header newBtn
    stack <- new Gtk.Stack [#transitionType := Gtk.StackTransitionTypeCrossfade]
    -- Home: the projects, as a grid of buttons.
    flow <- new Gtk.FlowBox [#selectionMode := Gtk.SelectionModeNone, #maxChildrenPerLine := 6, #columnSpacing := 12, #rowSpacing := 12, #margin := 24]
    homeScroll <- new Gtk.ScrolledWindow []
    Gtk.containerAdd homeScroll flow
    status <- Gtk.labelNew (Just "")
    home <- Gtk.boxNew Gtk.OrientationVertical 8
    Gtk.boxPackStart home status False False 8
    Gtk.boxPackStart home homeScroll True True 0
    -- Tools: one at a time, each with its kind; then colour, size and opacity.
    selectR <- Gtk.radioButtonNewWithLabel ([] :: [Gtk.RadioButton]) "Select"
    brushR <- Gtk.radioButtonNewWithLabelFromWidget (Just selectR) "Brush"
    scatterR <- Gtk.radioButtonNewWithLabelFromWidget (Just brushR) "Scatter"
    eraserR <- Gtk.radioButtonNewWithLabelFromWidget (Just brushR) "Eraser"
    shapeR <- Gtk.radioButtonNewWithLabelFromWidget (Just brushR) "Shapes"
    fillR <- Gtk.radioButtonNewWithLabelFromWidget (Just brushR) "Fill"
    textR <- Gtk.radioButtonNewWithLabelFromWidget (Just brushR) "Text"
    forM_ [selectR, brushR, scatterR, eraserR, shapeR, fillR, textR] $ \r -> Gtk.toggleButtonSetMode r False
    Gtk.toggleButtonSetActive brushR True
    brushC <- combo ["pen", "pencil", "marker", "paint"]
    scatterC <- combo ["grass", "birds", "stars", "leaves", "flowers", "spray"]
    eraserC <- combo ["pixel", "object"]
    shapeC <- combo ["ellipse", "rect", "triangle", "line"]
    filledB <- Gtk.checkButtonNewWithLabel "Filled"
    colourB <- Gtk.colorButtonNew
    setColour colourB "#1c1c1e"
    sizeS <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal 0.5 200 0.5
    Gtk.rangeSetValue sizeS 6
    Gtk.widgetSetSizeRequest sizeS 140 (-1)
    alphaS <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal 0.05 1 0.05
    Gtk.rangeSetValue alphaS 1
    Gtk.widgetSetSizeRequest alphaS 100 (-1)
    sizeL <- Gtk.labelNew (Just "Size")
    alphaL <- Gtk.labelNew (Just "Opacity")
    -- The tools stand in a rail beside the canvas; the bar above shows the chosen tool's
    -- options, then colour, size and opacity.
    rail <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginStart rail 6
    Gtk.widgetSetMarginTop rail 6
    pack rail [toW selectR, toW brushR, toW scatterR, toW eraserR, toW shapeR, toW fillR, toW textR]
    tools <- Gtk.boxNew Gtk.OrientationHorizontal 6
    Gtk.widgetSetMarginStart tools 8
    Gtk.widgetSetMarginEnd tools 8
    Gtk.widgetSetMarginTop tools 4
    pack tools [toW brushC, toW scatterC, toW eraserC, toW shapeC, toW filledB]
    toolsEnd <- sequence [toW colourB, toW sizeL, toW sizeS, toW alphaL, toW alphaS]
    forM_ (reverse toolsEnd) $ \w -> Gtk.boxPackEnd tools w False False 0
    options <- sequence [toW brushC, toW scatterC, toW eraserC, toW shapeC, toW filledB]
    let
        showOptions t = zipWithM_ Gtk.widgetSetVisible options $ case t of
            BrushTool -> [True, False, False, False, False]
            ScatterTool -> [False, True, False, False, False]
            EraserTool -> [False, False, True, False, False]
            ShapeTool -> [False, False, False, True, True]
            _ -> [False, False, False, False, False]
    -- Editor: tools over canvas over transport over timeline; the chat on the right.
    canvas <- new Gtk.DrawingArea [#canFocus := True]
    timeline <- new Gtk.DrawingArea []
    Gtk.widgetSetSizeRequest timeline (-1) 190
    forM_ [canvas, timeline] $ \a -> Gtk.widgetAddEvents a [Gdk.EventMaskButtonPressMask, Gdk.EventMaskButtonReleaseMask, Gdk.EventMaskPointerMotionMask, Gdk.EventMaskScrollMask, Gdk.EventMaskSmoothScrollMask]
    prevBtn <- Gtk.buttonNewWithLabel "◀"
    playBtn <- Gtk.buttonNewWithLabel "Play"
    nextBtn <- Gtk.buttonNewWithLabel "▶"
    onionBtn <- Gtk.toggleButtonNewWithLabel "Onion skin"
    Gtk.toggleButtonSetActive onionBtn True
    fitBtn <- Gtk.buttonNewWithLabel "Fit"
    counter <- Gtk.labelNew (Just "")
    newCelB <- Gtk.buttonNewWithLabel "+ Drawing"
    dupCelB <- Gtk.buttonNewWithLabel "Duplicate"
    delCelB <- Gtk.buttonNewWithLabel "Delete drawing"
    lessB <- Gtk.buttonNewWithLabel "Hold −"
    moreB <- Gtk.buttonNewWithLabel "Hold +"
    addLayerB <- Gtk.buttonNewWithLabel "+ Layer"
    hideB <- Gtk.buttonNewWithLabel "Hide layer"
    lockB <- Gtk.buttonNewWithLabel "Lock layer"
    delLayerB <- Gtk.buttonNewWithLabel "Delete layer"
    transport <- Gtk.boxNew Gtk.OrientationHorizontal 6
    Gtk.widgetSetMarginStart transport 8
    Gtk.widgetSetMarginEnd transport 8
    pack transport [toW prevBtn, toW playBtn, toW nextBtn, toW counter, toW newCelB, toW dupCelB, toW delCelB, toW lessB, toW moreB, toW addLayerB, toW hideB, toW lockB, toW delLayerB]
    Gtk.boxPackEnd transport fitBtn False False 0
    Gtk.boxPackEnd transport onionBtn False False 0
    left <- Gtk.boxNew Gtk.OrientationVertical 4
    work <- Gtk.boxNew Gtk.OrientationHorizontal 0
    Gtk.boxPackStart work rail False False 0
    Gtk.boxPackStart work canvas True True 0
    Gtk.boxPackStart left tools False False 0
    Gtk.boxPackStart left work True True 0
    Gtk.boxPackStart left transport False False 4
    Gtk.boxPackStart left timeline False False 0
    chat <- chatPlaceholder
    paned <- Gtk.panedNew Gtk.OrientationHorizontal
    Gtk.panedPack1 paned left True False
    Gtk.panedPack2 paned chat False False
    Gtk.panedSetPosition paned 1040
    Gtk.stackAddNamed stack home "home"
    Gtk.stackAddNamed stack paned "editor"
    Gtk.containerAdd win stack
    let
        redraw = Gtk.widgetQueueDraw canvas >> Gtk.widgetQueueDraw timeline >> showCounter
        showCounter = do
            s <- readIORef st
            case s.open of
                Nothing -> Gtk.labelSetText counter ""
                Just o -> Gtk.labelSetText counter (Text.pack (show (s.playhead + 1) <> " / " <> show (duration o.view)))
            Gtk.widgetSetSensitive undoBtn (not (null s.past))
            Gtk.widgetSetSensitive redoBtn (not (null s.future))
        say t = Gtk.labelSetText status t >> Gtk.labelSetText saveState t
        stop = do
            s <- readIORef st
            forM_ s.playing GLib.sourceDestroy
            writeIORef st s {playing = Nothing}
            Gtk.buttonSetLabel playBtn "Play"
        goHome = do
            stop
            saveNow
            modifyIORef' st (\s -> s {open = Nothing, past = [], future = []})
            Gtk.stackSetVisibleChildName stack "home"
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
                        forM_ ps $ \p -> do
                            btn <- Gtk.buttonNewWithLabel (unparse (summaryName p))
                            Gtk.widgetSetSizeRequest btn 200 120
                            _ <- on btn #clicked (openProject (summaryId p))
                            Gtk.containerAdd flow btn
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
                        Gtk.labelSetText saveState "Saved"
                        redraw
        connect c = do
            stop
            cacheDir <- cacheFolder
            b <- case c of
                Local folder -> localBackend folder
                Remote url hs -> remoteBackend url hs cacheDir
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
                    writeIORef st s {open = Just o', past = take 200 (o.project : s.past), future = [], dirty = True}
                    Gtk.labelSetText saveState "Saving…"
                    scheduleSave
                    redraw
        undo = do
            s <- readIORef st
            case (s.open, s.past) of
                (Just o, p : rest) -> do
                    o' <- reopen o p
                    writeIORef st s {open = Just o', past = rest, future = o.project : s.future, dirty = True}
                    scheduleSave >> redraw
                _ -> pure ()
        redo = do
            s <- readIORef st
            case (s.open, s.future) of
                (Just o, p : rest) -> do
                    o' <- reopen o p
                    writeIORef st s {open = Just o', future = rest, past = o.project : s.past, dirty = True}
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
        play = do
            s <- readIORef st
            case (s.playing, s.open) of
                (Just _, _) -> stop
                (Nothing, Just o) -> do
                    src <- GLib.timeoutSourceNew (fromIntegral (max 1 (1000 `div` o.view.rate)))
                    GLib.sourceSetCallback src (step 1 >> pure True)
                    _ <- GLib.sourceAttach src Nothing
                    writeIORef st s {playing = Just src}
                    Gtk.buttonSetLabel playBtn "Pause"
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
    _ <- on brushC #changed $ Gtk.comboBoxTextGetActiveText brushC >>= mapM_ (\v -> modifyIORef' st (\s -> s {brushKind = v})) >> Gtk.toggleButtonSetActive brushR True
    _ <- on scatterC #changed $ Gtk.comboBoxTextGetActiveText scatterC >>= mapM_ (\v -> modifyIORef' st (\s -> s {scatterKind = v})) >> Gtk.toggleButtonSetActive scatterR True
    _ <- on eraserC #changed $ Gtk.comboBoxTextGetActiveText eraserC >>= mapM_ (\v -> modifyIORef' st (\s -> s {objectEraser = v == "object"})) >> Gtk.toggleButtonSetActive eraserR True
    _ <- on shapeC #changed $ do
        v <- Gtk.comboBoxTextGetActiveText shapeC
        let
            k = case v of
                Just "rect" -> RectShape
                Just "triangle" -> TriangleShape
                Just "line" -> LineShape
                _ -> EllipseShape
        modifyIORef' st (\s -> s {shapeKind = k})
        Gtk.toggleButtonSetActive shapeR True
    _ <- on filledB #toggled (Gtk.toggleButtonGetActive filledB >>= \f -> modifyIORef' st (\s -> s {filled = f}))
    _ <- on colourB #colorSet (getColour colourB >>= \c -> modifyIORef' st (\s -> s {colour = c}))
    _ <- on sizeS #valueChanged (Gtk.rangeGetValue sizeS >>= \v -> modifyIORef' st (\s -> s {penSize = v}))
    _ <- on alphaS #valueChanged (Gtk.rangeGetValue alphaS >>= \v -> modifyIORef' st (\s -> s {penAlpha = v}))
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
        Connector.renderWithContext (drawCanvas s (finish s)) ctx
        pure True
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
                    writeIORef st s {drag = Nothing, draft = Nothing, selDrag = Nothing}
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
        pure True
    _ <- on timeline #scrollEvent $ \ev -> do
        dx <- get ev #deltaX
        dy <- get ev #deltaY
        modifyIORef' st (\s -> s {tlScroll = max 0 (s.tlScroll + (dx + dy) * 20)})
        Gtk.widgetQueueDraw timeline
        pure True
    -- Keys, as in the browser: tools, frames, drawings, undo.
    let
        onKey name cmd shift = do
            s <- readIORef st
            if isNothing s.open
                then pure False
                else do
                    case (cmd, Text.toLower name) of
                        (True, "z") -> (if shift then redo else undo) >> pure True
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
    _ <- on win #keyPressEvent $ \ev -> do
        kv <- get ev #keyval
        mods <- get ev #state
        name <- fromMaybe "" <$> Gdk.keyvalName kv
        onKey name (any (`elem` mods) [Gdk.ModifierTypeControlMask, Gdk.ModifierTypeMetaMask, Gdk.ModifierTypeSuperMask]) (Gdk.ModifierTypeShiftMask `elem` mods)
    _ <- on win #keyReleaseEvent $ \ev -> do
        kv <- get ev #keyval
        name <- fromMaybe "" <$> Gdk.keyvalName kv
        onKeyUp name
    _ <- on win #focusOutEvent $ \_ -> modifyIORef' st (\s -> s {spaceHeld = False, shiftHeld = False}) >> pure False
    Gtk.widgetShowAll win
    showOptions BrushTool
    let
        probe = do
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
                    }
        buttons = [("+ Drawing", newCelB), ("Duplicate", dupCelB), ("Delete drawing", delCelB), ("Hold +", moreB), ("Hold −", lessB), ("+ Layer", addLayerB), ("Hide layer", hideB), ("Lock layer", lockB), ("Delete layer", delLayerB), ("Undo", undoBtn), ("Redo", redoBtn), ("Play", playBtn), ("◀", prevBtn), ("▶", nextBtn), ("Fit", fitBtn)]
        clickNamed n = forM_ (lookup n buttons) Gtk.buttonClicked
    pure Window {window = win, canvasArea = canvas, connectTo = connect, openId = openProject, inspect = probe, say = say, pointer = onPointer, key = onKey, keyUp = onKeyUp, click = clickNamed}

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
        assets = [i.asset | LayerView {kind = DrawingView _ cels} <- v.stack, c <- cels, ImageV i <- c.items]
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
headerW = 150
rulerH = 22
rowH = 34
unitW = 14

-- | Widget pixels → canvas pixels.
canvasPoint :: State -> (Double, Double) -> (Double, Double)
canvasPoint s (x, y) = ((x - s.panX) / s.magnify, (y - s.panY) / s.magnify)

drawCanvas :: State -> (Draft -> Maybe (Either Text Item)) -> C.Render ()
drawCanvas s finishing = do
    C.setSourceRGB 0.16 0.16 0.18
    C.paint
    forM_ s.open $ \o -> do
        let
            v = o.view
            pw = fromIntegral v.width
            ph = fromIntegral v.height
        C.translate s.panX s.panY
        C.scale s.magnify s.magnify
        C.setSourceRGB 1 1 1
        C.rectangle 0 0 pw ph
        C.fill
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
        -- The selection's box, its corner handles (scale) and the round handle above (turn).
        forM_ (here >>= \n -> boxOf (selected n)) $ \bx -> forM_ (lookup s.layer (zip [0 ..] v.stack)) $ \l -> do
            let
                (m, _) = placement l s.playhead
                px = 1 / s.magnify
            C.save
            C.transform m
            forM_ s.selDrag (C.transform . (.current))
            C.setSourceRGB 0.18 0.42 1
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
    C.setSourceRGB 0.12 0.12 0.13
    C.paint
    forM_ s.open $ \o -> do
        let
            v = o.view
            total = duration v
            x0 = headerW - s.tlScroll
            frameX f = x0 + fromIntegral f * unitW
            layers = reverse (zip [0 :: Int ..] v.stack)
        -- Ruler: a tick a frame, a longer one a second.
        C.setSourceRGB 0.55 0.55 0.6
        forM_ [0 .. total + 24] $ \f -> do
            let
                x = frameX f
                second = f `mod` max 1 v.rate == 0
            when (x >= headerW && x <= w) $ do
                C.rectangle x (if second then 8 else 15) 1 (if second then 14 else 7)
                C.fill
        -- Rows: drawings as blocks as wide as they are held.
        forM_ (zip [0 :: Int ..] layers) $ \(row, (li, l)) -> do
            let
                y = rulerH + fromIntegral row * rowH
            when (li == s.layer) $ do
                C.setSourceRGBA 0.36 0.55 1 0.12
                C.rectangle 0 y w rowH
                C.fill
            case l.kind of
                DrawingView holdLast cels -> do
                    let
                        here = celAt holdLast cels s.playhead
                        starts = scanl (+) 0 (map (.hold) cels)
                    forM_ (zip3 [0 :: Int ..] cels starts) $ \(ci, cel, start) -> do
                        let
                            x = frameX start
                            cw = fromIntegral cel.hold * unitW - 2
                        unless (x + cw < headerW || x > w) $ do
                            if li == s.layer && here == Just ci
                                then C.setSourceRGB 0.36 0.55 1
                                else if null cel.items then C.setSourceRGB 0.25 0.25 0.28 else C.setSourceRGB 0.85 0.86 0.9
                            C.rectangle (max headerW x) (y + 5) (min cw (x + cw - headerW)) (rowH - 10)
                            C.fill
                LightLayerView _ -> band y (1, 0.85, 0.5)
                ReferenceView _ _ -> band y (0.6, 0.6, 0.65)
            -- Layer name column, over the lane when it scrolls.
            C.setSourceRGB 0.15 0.15 0.17
            C.rectangle 0 y headerW rowH
            C.fill
            C.setSourceRGB (if l.visible then 0.92 else 0.5) (if l.visible then 0.92 else 0.5) (if l.visible then 0.95 else 0.55)
            C.moveTo 10 (y + rowH / 2 + 4)
            C.setFontSize 12
            C.showText (Text.unpack (Text.take 16 l.name <> (if l.locked then " 🔒" else "")))
        -- Playhead.
        let
            px = frameX s.playhead + unitW / 2
        when (px >= headerW) $ do
            C.setSourceRGB 1 0.35 0.35
            C.rectangle px 0 1.5 h
            C.fill
    where
        band y (r, g, b) = do
            C.setSourceRGBA r g b 0.25
            C.rectangle headerW (y + 5) (w - headerW) (rowH - 10)
            C.fill

-- Widgets ---------------------------------------------------------------------------------

toW :: (Gtk.IsWidget w) => w -> IO Gtk.Widget
toW = Gtk.toWidget

pack :: Gtk.Box -> [IO Gtk.Widget] -> IO ()
pack box ws = sequence ws >>= mapM_ (\w -> Gtk.boxPackStart box w False False 0)

combo :: [Text] -> IO Gtk.ComboBoxText
combo opts = do
    c <- Gtk.comboBoxTextNew
    mapM_ (Gtk.comboBoxTextAppendText c) opts
    Gtk.comboBoxSetActive c 0
    pure c

setColour :: Gtk.ColorButton -> Text -> IO ()
setColour b hex = do
    rgba <- new Gdk.RGBA []
    ok <- Gdk.rGBAParse rgba hex
    when ok (Gtk.colorChooserSetRgba b rgba)

-- | The chosen colour as #rrggbb.
getColour :: Gtk.ColorButton -> IO Text
getColour b = do
    rgba <- Gtk.colorChooserGetRgba b
    r <- Gdk.getRGBARed rgba
    g <- Gdk.getRGBAGreen rgba
    bl <- Gdk.getRGBABlue rgba
    let
        byte v = let n = max 0 (min 255 (round (v * 255) :: Int)) in (if n < 16 then "0" else "") <> showHex n ""
    pure (Text.pack ("#" <> byte r <> byte g <> byte bl))

chatPlaceholder :: IO Gtk.Box
chatPlaceholder = do
    box <- Gtk.boxNew Gtk.OrientationVertical 8
    Gtk.widgetSetSizeRequest box 300 (-1)
    title <- Gtk.labelNew (Just "Chat")
    note <- Gtk.labelNew (Just "Chat with a model about this project comes next: it will draw, light and animate through the same tools agents use.")
    Gtk.labelSetLineWrap note True
    Gtk.labelSetMaxWidthChars note 36
    Gtk.boxPackStart box title False False 12
    Gtk.boxPackStart box note False False 0
    pure box

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
    d <- new Gtk.Dialog [#title := "Where projects live", #modal := True, #transientFor := parent, #defaultWidth := 520]
    area <- Gtk.dialogGetContentArea d
    folder0 <- defaultFolder
    local <- Gtk.radioButtonNewWithLabel ([] :: [Gtk.RadioButton]) "On this computer, in this folder:"
    folderEntry <- Gtk.entryNew
    Gtk.entrySetText folderEntry (Text.pack (case current of Just (Local f) -> f; _ -> folder0))
    remote <- Gtk.radioButtonNewWithLabelFromWidget (Just local) "On my anima2D server, at this address:"
    urlEntry <- Gtk.entryNew
    Gtk.entrySetPlaceholderText urlEntry (Just "https://anim.example.com")
    hint <- Gtk.labelNew (Just "Headers to send with every request, one per line as Name: value (an access token for a sign-in gate, for example):")
    Gtk.labelSetLineWrap hint True
    headers <- Gtk.textViewNew
    Gtk.widgetSetSizeRequest headers (-1) 70
    case current of
        Just (Remote u hs) -> do
            Gtk.toggleButtonSetActive remote True
            Gtk.entrySetText urlEntry u
            buf <- Gtk.textViewGetBuffer headers
            Gtk.textBufferSetText buf (Text.intercalate "\n" [k <> ": " <> v | (k, v) <- hs]) (-1)
        _ -> Gtk.toggleButtonSetActive local True
    widgets <- sequence [toW local, toW folderEntry, toW remote, toW urlEntry, toW hint, toW headers]
    forM_ widgets $ \wdg -> do
        Gtk.widgetSetMarginStart wdg 12
        Gtk.widgetSetMarginEnd wdg 12
        Gtk.boxPackStart area wdg False False 4
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
                        let
                            hs =
                                [ (Text.strip k, Text.strip (Text.drop 1 v))
                                | ln <- Text.lines txt
                                , let
                                    (k, v) = Text.breakOn ":" ln
                                , not (Text.null (Text.strip k))
                                , not (Text.null v)
                                ]
                        pure (Just (Remote (Text.strip u) hs))
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
