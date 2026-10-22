{- | The window: the project list, and the editor (canvas, timeline, playback, onion skin)
with the chat sidebar beside it. GTK runs on the main thread; anything slow (the network,
the disk) runs on its own thread and hands the result back with 'GLib.idleAdd'.
-}
module Anima.Desktop.Ui (runApp) where

import Anima.Ids (newUuid)
import Anima.Types.Parse (Parse (..))
import Anima.Types.Project hiding (duration)
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, unless, void, when)
import Data.GI.Base (AttrOp (..), get, new, on)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Text (Text)

import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Connector qualified as Connector
import GI.GLib qualified as GLib
import GI.Gdk qualified as Gdk
import GI.GdkPixbuf qualified as Pixbuf
import GI.Gtk qualified as Gtk

import Anima.Desktop.Backend (Backend (..), Saved (..), localBackend, remoteBackend)
import Anima.Desktop.Config (Connection (..), cacheFolder, defaultFolder, loadConnection, saveConnection)
import Anima.Desktop.Draw (Pictures, drawCel)
import Anima.Desktop.Scene (Env (..), celAt, drawLayers, duration, newCache)

-- State ----------------------------------------------------------------------------------

data Open = Open
    { pid :: ProjectId
    , project :: Project
    , view :: ProjectView
    , revision :: Revision
    , env :: Env
    }

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
    , seq_ :: Maybe Int
    }

initial :: State
initial = State {backend = Nothing, open = Nothing, playhead = 0, layer = 0, playing = Nothing, onion = True, magnify = 1, panX = 0, panY = 0, fitted = False, drag = Nothing, tlScroll = 0, seq_ = Nothing}

-- | Run on the GTK thread.
onMain :: IO () -> IO ()
onMain act = void (GLib.idleAdd GLib.PRIORITY_DEFAULT (act >> pure False))

background :: IO a -> (a -> IO ()) -> IO ()
background work done = void . forkIO $ do
    r <- work
    onMain (done r)

-- App -------------------------------------------------------------------------------------

runApp :: IO ()
runApp = do
    _ <- Gtk.init Nothing
    st <- newIORef initial
    win <- new Gtk.Window [#title := "anima2D", #defaultWidth := 1320, #defaultHeight := 860]
    _ <- on win #destroy Gtk.mainQuit
    header <- new Gtk.HeaderBar [#showCloseButton := True, #title := "anima2D"]
    Gtk.windowSetTitlebar win (Just header)
    backBtn <- Gtk.buttonNewWithLabel "Projects"
    connBtn <- Gtk.buttonNewWithLabel "Connection…"
    newBtn <- Gtk.buttonNewWithLabel "New project"
    Gtk.headerBarPackStart header backBtn
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
    -- Editor: canvas over transport over timeline; the chat on the right.
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
    transport <- Gtk.boxNew Gtk.OrientationHorizontal 6
    Gtk.widgetSetMarginStart transport 8
    Gtk.widgetSetMarginEnd transport 8
    forM_ [prevBtn, playBtn, nextBtn] $ \b -> Gtk.boxPackStart transport b False False 0
    Gtk.boxPackStart transport counter False False 12
    Gtk.boxPackEnd transport fitBtn False False 0
    Gtk.boxPackEnd transport onionBtn False False 0
    left <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.boxPackStart left canvas True True 0
    Gtk.boxPackStart left transport False False 4
    Gtk.boxPackStart left timeline False False 0
    chat <- chatPlaceholder
    paned <- Gtk.panedNew Gtk.OrientationHorizontal
    Gtk.panedPack1 paned left True False
    Gtk.panedPack2 paned chat False True
    Gtk.panedSetPosition paned 1000
    Gtk.stackAddNamed stack home "home"
    Gtk.stackAddNamed stack paned "editor"
    Gtk.containerAdd win stack
    let
        redraw = Gtk.widgetQueueDraw canvas >> Gtk.widgetQueueDraw timeline >> showCounter
        showCounter = do
            s <- readIORef st
            case s.open of
                Nothing -> Gtk.labelSetText counter ""
                Just o -> Gtk.labelSetText counter (Text.pack (show (s.playhead + 1) <> " / " <> show (duration o.view) <> " · " <> show o.view.rate <> " fps"))
        say = Gtk.labelSetText status
        stop = do
            s <- readIORef st
            forM_ s.playing GLib.sourceDestroy
            writeIORef st s {playing = Nothing}
            Gtk.buttonSetLabel playBtn "Play"
        goHome = do
            stop
            modifyIORef' st (\s -> s {open = Nothing})
            Gtk.stackSetVisibleChildName stack "home"
            Gtk.headerBarSetSubtitle header Nothing
            refreshList
        refreshList = do
            s <- readIORef st
            forM_ s.backend $ \b -> do
                say ("Loading projects from " <> b.describe <> "…")
                background b.list $ \case
                    Left why -> say ("The projects could not be listed: " <> why)
                    Right ps -> do
                        kids <- Gtk.containerGetChildren flow
                        mapM_ Gtk.widgetDestroy kids
                        say (if null ps then "No projects yet: start one with New project." else b.describe)
                        forM_ ps $ \p -> do
                            btn <- Gtk.buttonNewWithLabel (unparse (summaryName p))
                            Gtk.widgetSetSizeRequest btn 200 120
                            _ <- on btn #clicked (openProject (summaryId p))
                            Gtk.containerAdd flow btn
                        Gtk.widgetShowAll flow
        openProject pid = do
            s <- readIORef st
            forM_ s.backend $ \b -> do
                say "Opening…"
                background (b.load pid) $ \case
                    Left why -> say ("It could not be opened: " <> why)
                    Right (p, rev) -> do
                        o <- opened b pid p rev
                        modifyIORef' st (\x -> x {open = Just o, playhead = 0, layer = topDrawing o.view, fitted = False})
                        Gtk.headerBarSetSubtitle header (Just o.view.title)
                        Gtk.stackSetVisibleChildName stack "editor"
                        redraw
        connect c = do
            stop
            cacheDir <- cacheFolder
            b <- case c of
                Local folder -> localBackend folder
                Remote url hs -> remoteBackend url hs cacheDir
            modifyIORef' st (\s -> s {backend = Just b, open = Nothing, seq_ = Nothing})
            goHome
            watch b
        -- Live updates: wait on the backend for saves; reload the open project when it changed.
        watch b = void . forkIO $ do
            let
                loop after = do
                    n <- b.changes after
                    s <- readIORef st
                    case (n, fmap (.describe) s.backend) of
                        (_, Just d) | d /= b.describe -> pure ()
                        (Nothing, _) -> GLib.usleep 30000000 >> loop Nothing
                        (Just k, _) -> do
                            when (isJust after && n /= after) (onMain reloadOpen)
                            loop (Just k)
            loop Nothing
        reloadOpen = do
            s <- readIORef st
            case (s.backend, s.open) of
                (Just b, Just o) -> background (b.load o.pid) $ \case
                    Right (p, rev) | rev /= o.revision -> do
                        o' <- opened b o.pid p rev
                        modifyIORef' st (\x -> x {open = Just o'})
                        redraw
                    _ -> pure ()
                (Just _, Nothing) -> refreshList
                _ -> pure ()
        step d = do
            s <- readIORef st
            forM_ s.open $ \o -> do
                let
                    n = duration o.view
                writeIORef st s {playhead = (s.playhead + d) `mod` max 1 n}
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
    _ <- on backBtn #clicked goHome
    _ <- on prevBtn #clicked (stop >> step (-1))
    _ <- on nextBtn #clicked (stop >> step 1)
    _ <- on playBtn #clicked play
    _ <- on onionBtn #toggled $ do
        on' <- Gtk.toggleButtonGetActive onionBtn
        modifyIORef' st (\s -> s {onion = on'})
        redraw
    _ <- on fitBtn #clicked (modifyIORef' st (\s -> s {fitted = False}) >> redraw)
    _ <- on connBtn #clicked $ do
        current <- loadConnection
        chosen <- connectionDialog win current
        forM_ chosen $ \c -> saveConnection c >> connect c
    _ <- on newBtn #clicked $ do
        s <- readIORef st
        forM_ s.backend $ \b -> do
            name <- askName win
            forM_ name $ \nm -> background (createProject b nm) $ \case
                Left why -> say ("It could not be made: " <> why)
                Right pid -> openProject pid
    -- The canvas: drawn from the open project; wheel zooms about the pointer, drag pans.
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
        Connector.renderWithContext (drawCanvas s) ctx
        pure True
    _ <- on canvas #scrollEvent $ \ev -> do
        dy <- get ev #deltaY
        dir <- get ev #direction
        x <- get ev #x
        y <- get ev #y
        let
            amount = case dir of
                Gdk.ScrollDirectionUp -> -1
                Gdk.ScrollDirectionDown -> 1
                _ -> dy
            f = exp (-(amount * 0.12))
        modifyIORef' st $ \s ->
            let
                z = max 0.05 (min 32 (s.magnify * f))
                r = z / s.magnify
            in
                s {magnify = z, panX = x - (x - s.panX) * r, panY = y - (y - s.panY) * r}
        Gtk.widgetQueueDraw canvas
        pure True
    _ <- on canvas #buttonPressEvent $ \ev -> do
        x <- get ev #x
        y <- get ev #y
        modifyIORef' st (\s -> s {drag = Just (x, y, s.panX, s.panY)})
        pure True
    _ <- on canvas #motionNotifyEvent $ \ev -> do
        x <- get ev #x
        y <- get ev #y
        s <- readIORef st
        forM_ s.drag $ \(x0, y0, px, py) -> do
            writeIORef st s {panX = px + x - x0, panY = py + y - y0}
            Gtk.widgetQueueDraw canvas
        pure True
    _ <- on canvas #buttonReleaseEvent $ \_ -> modifyIORef' st (\s -> s {drag = Nothing}) >> pure True
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
        forM_ s.open $ \o -> when (x > headerW) $ do
            let
                f = max 0 (min (duration o.view - 1) (floor ((x - headerW + s.tlScroll) / unitW)))
                row = floor ((y - rulerH) / rowH) :: Int
                layers = reverse (zip [0 ..] o.view.stack)
                picked = if y > rulerH then fmap fst (lookup row (zip [0 ..] layers)) else Nothing
            stop
            writeIORef st s {playhead = f, layer = fromMaybe s.layer picked}
            redraw
        pure True
    _ <- on timeline #scrollEvent $ \ev -> do
        dx <- get ev #deltaX
        dy <- get ev #deltaY
        modifyIORef' st (\s -> s {tlScroll = max 0 (s.tlScroll + (dx + dy) * 20)})
        Gtk.widgetQueueDraw timeline
        pure True
    Gtk.widgetShowAll win
    -- First run asks where projects live; after that the choice is remembered.
    saved <- loadConnection
    c <- case saved of
        Just c -> pure (Just c)
        Nothing -> do
            r <- connectionDialog win Nothing
            forM_ r saveConnection
            pure r
    case c of
        Just conn -> connect conn
        Nothing -> Gtk.labelSetText status "Choose where projects live with Connection…"
    Gtk.main

-- Opening ---------------------------------------------------------------------------------

opened :: Backend -> ProjectId -> Project -> Revision -> IO Open
opened b pid p rev = do
    let
        v = viewProject p
    pics <- loadPictures b v
    cache <- newCache
    pure Open {pid, project = p, view = v, revision = rev, env = Env {pictures = pics, cache, project = v}}

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

-- Drawing ---------------------------------------------------------------------------------

headerW, rulerH, rowH, unitW :: Double
headerW = 150
rulerH = 22
rowH = 34
unitW = 14

drawCanvas :: State -> C.Render ()
drawCanvas s = do
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
        drawLayers o.env s.playhead [i | (i, l) <- zip [0 ..] v.stack, not (isReference l)] Nothing Nothing
    where
        isReference l = case l.kind of
            ReferenceView _ _ -> True
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
            C.showText (Text.unpack (Text.take 18 l.name))
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

-- Dialogs ---------------------------------------------------------------------------------

chatPlaceholder :: IO Gtk.Box
chatPlaceholder = do
    box <- Gtk.boxNew Gtk.OrientationVertical 8
    Gtk.widgetSetSizeRequest box 300 (-1)
    title <- Gtk.labelNew (Just "Chat")
    note <- Gtk.labelNew (Just "Chat with a model about this project comes in the next step: it will draw, light and animate through the same tools agents use.")
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
    widgets <- sequence [Gtk.toWidget local, Gtk.toWidget folderEntry, Gtk.toWidget remote, Gtk.toWidget urlEntry, Gtk.toWidget hint, Gtk.toWidget headers]
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

askName :: Gtk.Window -> IO (Maybe Text)
askName parent = do
    d <- new Gtk.Dialog [#title := "New project", #modal := True, #transientFor := parent]
    area <- Gtk.dialogGetContentArea d
    e <- Gtk.entryNew
    Gtk.entrySetText e "Untitled"
    Gtk.entrySetActivatesDefault e True
    Gtk.boxPackStart area e False False 12
    ok <- dialogRun d
    t <- Gtk.entryGetText e
    Gtk.widgetDestroy d
    pure (if ok && not (Text.null (Text.strip t)) then Just (Text.strip t) else Nothing)
