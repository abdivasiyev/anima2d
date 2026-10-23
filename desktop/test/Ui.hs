{- | The window, driven as a person would: the real window is built, a project is made in a
folder, and the pen, the keys and the buttons go through the same functions GTK's events
do. After each step the project is checked, and the window is drawn to PNGs (in
@ui-snapshots/@) to look at. It draws offscreen, but GTK still wants a login session.
-}
module Main (main) where

import Anima.Ids (newUuid)
import Anima.Types.Project
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless, void, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Clock (getMonotonicTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, getFileSize, getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Connector qualified as Connector
import GI.Gtk qualified as Gtk

import Anima.Desktop.Assistant (Model (..), Reply (..), Stop (..), ToolUse (..))
import Anima.Desktop.Backend (Backend (..), localBackend)
import Anima.Desktop.Config (Connection (..))
import Anima.Desktop.Export (Export (..), Fit (..), Format (..), Quality (..), Size (..))
import Anima.Desktop.Ui

-- | Runs GTK's loop for `ms` milliseconds, so idle callbacks, timers and saves happen.
pump :: Double -> IO ()
pump ms = do
    t0 <- getMonotonicTime
    let
        loop = do
            pending <- Gtk.eventsPending
            if pending then void (Gtk.mainIterationDo False) else threadDelay 2000
            t <- getMonotonicTime
            when ((t - t0) * 1000 < ms) loop
    loop

-- | Pumps until the window satisfies `ok`, for at most `ms`.
waitFor :: Window -> Double -> (Probe -> Bool) -> IO Probe
waitFor w ms ok = do
    t0 <- getMonotonicTime
    let
        loop = do
            pump 20
            p <- w.inspect
            t <- getMonotonicTime
            if ok p || (t - t0) * 1000 > ms then pure p else loop
    loop

snapshot :: (Gtk.IsWidget a) => a -> FilePath -> IO ()
snapshot widget path = do
    wd <- Gtk.widgetGetAllocatedWidth widget
    ht <- Gtk.widgetGetAllocatedHeight widget
    surf <- C.createImageSurface C.FormatARGB32 (fromIntegral (max 1 wd)) (fromIntegral (max 1 ht))
    C.renderWith surf (Connector.toRender (Gtk.widgetDraw widget))
    C.surfaceWriteToPNG surf path

{- | Moves the window's title bar and contents into an offscreen window, which GTK maps and
draws without a screen; it is returned, ready to snapshot.
-}
offscreen :: Gtk.Window -> IO Gtk.OffscreenWindow
offscreen win = do
    bar <- Gtk.windowGetTitlebar win
    body <- Gtk.binGetChild win
    whole <- Gtk.boxNew Gtk.OrientationVertical 0
    forM_ bar $ \b -> do
        Gtk.windowSetTitlebar win (Nothing :: Maybe Gtk.Widget)
        Gtk.boxPackStart whole b False False 0
    forM_ body $ \b -> do
        Gtk.containerRemove win b
        Gtk.boxPackStart whole b True True 0
    off <- Gtk.offscreenWindowNew
    Gtk.windowSetDefaultSize off 1360 880
    Gtk.widgetSetSizeRequest whole 1360 880
    Gtk.containerAdd off whole
    Gtk.widgetShowAll off
    Gtk.widgetHide win
    pure off

-- | The kinds of the items on drawing 1 of the layer the window edits.
kinds :: Probe -> [Text]
kinds p = case p.edited of
    Nothing -> []
    Just pr -> case drop p.onLayer (viewProject pr).stack of
        LayerView {kind = DrawingView _ (c : _)} : _ -> map kindOf c.items
        _ -> []
    where
        kindOf = \case
            StrokeV _ -> "stroke"
            EraseV _ -> "erase"
            ScatterV _ -> "scatter"
            ShapeV _ -> "shape"
            CurveV _ -> "curve"
            FillV _ -> "fill"
            TextV _ -> "text"
            ImageV _ -> "image"

refCount :: Probe -> Int
refCount p = maybe 0 (\pr -> length (viewProject pr).board) p.edited

follows :: Probe -> [Bool]
follows p = maybe [] (\pr -> map (.follows) (viewProject pr).board) p.edited

bottomIsReference :: Probe -> Bool
bottomIsReference p = case viewProject <$> p.edited of
    Just v | LayerView {kind = ReferenceView _ _} : _ <- v.stack -> True
    _ -> False

shapeCentre :: Probe -> Maybe (Double, Double)
shapeCentre p = case p.edited of
    Just pr | LayerView {kind = DrawingView _ (c : _)} : _ <- drop p.onLayer (viewProject pr).stack -> case [s.center | ShapeV s <- c.items] of
        x : _ -> Just x
        [] -> Nothing
    _ -> Nothing

main :: IO ()
main = do
    _ <- Gtk.init Nothing
    tmp <- getTemporaryDirectory
    run <- newUuid
    let
        dir = tmp </> ("anima2d-ui-" <> Text.unpack run)
        shots = "ui-snapshots"
    createDirectoryIfMissing True dir
    createDirectoryIfMissing True shots
    failures <- newIORef (0 :: Int)
    w <- buildWindow
    whole <- offscreen w.window
    let
        check what ok detail = do
            putStrLn ((if ok then "ok   " else "FAIL ") <> what <> (if ok then "" else ": " <> detail))
            unless ok (modifyIORef' failures (+ 1))
        -- Canvas coordinates to widget pixels, through the window's own zoom and pan.
        at (cx, cy) = do
            p <- w.inspect
            pure (p.screenOf (cx, cy))
        press phase c = do
            (x, y) <- at c
            void (w.pointer Pointer {phase = phase, px = x, py = y, pressure = 0.6, button = 1, panning = False, held = phase == Move})
        drag a@(ax, ay) (bx, by) = do
            press Press a
            forM_ [1 .. 16 :: Int] $ \i -> let k = fromIntegral i / 16 in press Move (ax + (bx - ax) * k, ay + (by - ay) * k)
            press Release (bx, by)
            pump 30
        tap c = press Press c >> press Release c >> pump 30
        keyed name = void (w.key name False False) >> pump 30
        state = w.inspect
    -- A project in a folder of its own, opened as from the list.
    b <- localBackend dir
    made <- createProject b "UI test"
    pid <- either (\e -> putStrLn ("could not make the project: " <> Text.unpack e) >> exitFailure) pure made
    w.connectTo (Local dir)
    _ <- waitFor w 1500 (const False)
    snapshot whole (shots </> "00-projects.png")
    w.openId pid
    opened <- waitFor w 5000 (\p -> isJust p.edited)
    check "the project opens" (isJust opened.edited) (Text.unpack opened.note)
    _ <- waitFor w 2000 (const False)
    cw <- Gtk.widgetGetAllocatedWidth w.canvasArea
    ch <- Gtk.widgetGetAllocatedHeight w.canvasArea
    check "the canvas has room" (cw > 200 && ch > 200) (show (cw, ch))
    snapshot whole (shots </> "01-opened.png")
    -- The brush.
    keyed "b"
    drag (400, 400) (800, 520)
    s1 <- state
    check "the brush draws a stroke" (kinds s1 == ["stroke"]) (show (kinds s1))
    -- A shape, then the bucket inside it.
    keyed "u"
    drag (1000, 300) (1400, 700)
    keyed "k"
    tap (1200, 500)
    s2 <- state
    check "a shape, then a fill inside it" (kinds s2 == ["stroke", "shape", "fill"]) (show (kinds s2) <> " " <> Text.unpack s2.note)
    -- Undo and redo, from the keys.
    void (w.key "z" True False) >> pump 30
    s3 <- state
    check "⌘Z undoes the fill" (kinds s3 == ["stroke", "shape"]) (show (kinds s3))
    void (w.key "z" True True) >> pump 30
    s4 <- state
    check "⇧⌘Z brings it back" (kinds s4 == ["stroke", "shape", "fill"]) (show (kinds s4))
    -- Select the shape and move it.
    keyed "v"
    drag (1200, 500) (1300, 550)
    s5 <- state
    let
        moved = case shapeCentre s5 of
            Just (x, y) -> abs (x - 1300) < 1 && abs (y - 550) < 1
            Nothing -> False
    check "Select picks the shape and moves it" (s5.chosen == [1] && moved) (show (s5.chosen, shapeCentre s5))
    snapshot w.canvasArea (shots </> "02-selected.png")
    void (w.key "delete" False False) >> pump 30
    s6 <- state
    check "Delete removes the selection" (kinds s6 == ["stroke", "fill"] && null s6.chosen) (show (kinds s6, s6.chosen))
    void (w.key "a" True False) >> pump 30
    s7 <- state
    check "⌘A selects the drawing" (s7.chosen == [0, 1] && s7.toolName == "select") (show (s7.chosen, s7.toolName))
    keyed "escape"
    s8 <- state
    check "Escape clears it" (null s8.chosen) (show s8.chosen)
    -- The pixel eraser.
    keyed "e"
    drag (500, 420) (700, 500)
    s9 <- state
    check "the eraser rubs out" (kinds s9 == ["stroke", "fill", "erase"]) (show (kinds s9))
    snapshot w.canvasArea (shots </> "03-drawn.png")
    -- Drawings, frames and layers.
    w.click "+ Drawing" >> pump 30
    void (w.key "right" False False) >> pump 30
    s10 <- state
    let
        frames = maybe 0 (\pr -> sum [length cels | LayerView {kind = DrawingView _ cels} <- (viewProject pr).stack]) s10.edited
    check "+ Drawing adds one, → moves to it" (frames == 2 && s10.atFrame == 1) (show (frames, s10.atFrame))
    void (w.key "right" False False) >> pump 30
    s11 <- state
    check "→ past the last frame wraps to the first" (s11.atFrame == 0) (show s11.atFrame)
    w.click "+ Layer" >> pump 30
    s12 <- state
    let
        layers = maybe 0 (\pr -> length (viewProject pr).stack) s12.edited
    check "+ Layer adds a layer and edits it" (layers == 2 && s12.onLayer == 1) (show (layers, s12.onLayer))
    -- The reference board: a picture, traced onto the canvas; a video that follows.
    let
        picture = dir </> "reference.png"
    sample <- C.createImageSurface C.FormatARGB32 320 180
    C.renderWith sample $ do
        C.setSourceRGB 0.95 0.62 0.36
        C.paint
        C.setSourceRGB 0.28 0.36 0.62
        C.rectangle 0 120 320 60
        C.fill
    C.surfaceWriteToPNG sample picture
    w.addReferenceFile picture
    r1 <- waitFor w 5000 (\p -> p.boardShows)
    check "a picture goes on the board and shows" (refCount r1 == 1 && r1.boardShows) (show (refCount r1, r1.boardShows, r1.note))
    snapshot whole (shots </> "05-board.png")
    layersBefore <- maybe 0 (\pr -> length (viewProject pr).stack) . (.edited) <$> state
    w.click "Trace" >> pump 200
    r2 <- state
    check "Trace puts it under the drawings" (maybe 0 (\pr -> length (viewProject pr).stack) r2.edited == layersBefore + 1 && bottomIsReference r2) (show (maybe 0 (\pr -> length (viewProject pr).stack) r2.edited))
    snapshot w.canvasArea (shots </> "06-traced.png")
    ffmpeg <- findExecutable "ffmpeg"
    case ffmpeg of
        Nothing -> putStrLn "skip a video on the board: no ffmpeg on the PATH"
        Just exe -> do
            let
                video = dir </> "reference.mp4"
            _ <- readProcessWithExitCode exe ["-loglevel", "error", "-y", "-loop", "1", "-i", picture, "-t", "2", "-r", "24", "-pix_fmt", "yuv420p", video] ""
            w.addReferenceFile video
            r3 <- waitFor w 8000 (\p -> refCount p == 2 && p.boardShows)
            check "a video goes on the board and shows its still" (refCount r3 == 2 && r3.boardShows) (show (refCount r3, r3.boardShows, r3.note))
            w.click "Follow" >> pump 100
            r4 <- state
            check "Follow switches it to play on its own" (follows r4 == [True, False]) (show (follows r4))
            snapshot whole (shots </> "07-video.png")
    w.click "Remove reference" >> pump 100
    r5 <- state
    check "Remove takes it off the board" (refCount r5 == (if isJust ffmpeg then 1 else 0)) (show (refCount r5))
    -- The assistant, with a scripted model: it reads what is on screen, draws a circle,
    -- looks at the frame, and answers; each change is an ordinary undoable edit.
    replies <- newIORef (0 :: Int)
    let
        circle = Aeson.object ["kind" Aeson..= ("shape" :: Text), "layer" Aeson..= ("line" :: Text), "shape" Aeson..= ("ellipse" :: Text), "color" Aeson..= ("#c2413b" :: Text), "size" Aeson..= (6 :: Int), "opacity" Aeson..= (1 :: Int), "fill" Aeson..= ("#f3c843" :: Text), "center" Aeson..= [960 :: Int, 300], "width" Aeson..= (240 :: Int), "height" Aeson..= (240 :: Int), "rotation" Aeson..= (0 :: Int)]
        reply said calls stop = Reply {said = said, calls = calls, stop = stop, keep = Aeson.object ["role" Aeson..= ("assistant" :: Text), "content" Aeson..= said]}
        script n = case n of
            0 -> reply "A sun, then." [ToolUse "t1" "editor_state" (Aeson.object []), ToolUse "t2" "add_items" (Aeson.object ["drawing" Aeson..= (1 :: Int), "items" Aeson..= [circle]])] WantsTools
            1 -> reply "" [ToolUse "t3" "preview" (Aeson.object ["frame" Aeson..= (1 :: Int), "width" Aeson..= (320 :: Int)]), ToolUse "t4" "add_items" (Aeson.object ["drawing" Aeson..= (1 :: Int), "items" Aeson..= [Aeson.object ["kind" Aeson..= ("blob" :: Text)]]])] WantsTools
            _ -> reply "Here is the sun, top centre." [] Done
        scripted =
            Model
                { label = "scripted model"
                , ask = \_ _ _ -> do
                    n <- readIORef replies
                    modifyIORef' replies (+ 1)
                    pure (Right (script n))
                , userTurn = \t -> [Aeson.object ["role" Aeson..= ("user" :: Text), "content" Aeson..= t]]
                , resultTurn = \outs ->
                    [Aeson.object ["role" Aeson..= ("user" :: Text), "content" Aeson..= length outs] | not (null outs)]
                }
    w.connectAssistant (Just scripted)
    itemsBefore <- length . kinds <$> state
    w.click "+ Layer" >> pump 30
    w.tellAssistant "Draw a sun"
    c1 <- waitFor w 8000 (\p -> not p.chatBusy && length p.chatLog >= 4)
    let
        roles = map fst c1.chatLog
        said = [t | ("assistant", t) <- c1.chatLog]
        steps = [t | ("step", t) <- c1.chatLog]
        refused = [t | ("failed", t) <- c1.chatLog]
    check "the assistant draws, looks, and answers" (take 1 roles == ["user"] && said == ["A sun, then.", "Here is the sun, top centre."] && length steps == 3) (show c1.chatLog)
    check "its circle is on the drawing" ("shape" `elem` kinds c1) (show (kinds c1))
    check "a rejected item comes back as the parser's words" (any ("items: 0: kind:" `Text.isInfixOf`) refused) (show refused)
    snapshot whole (shots </> "08-assistant.png")
    void (w.key "z" True False) >> pump 30
    c2 <- state
    check "⌘Z undoes the assistant's change" ("shape" `notElem` kinds c2) (show (kinds c2, itemsBefore))
    -- The layer panel: rename, opacity (a drag is one undo step), blend, depth, a shadow.
    p0 <- state
    let
        li = p0.onLayer
        layerOf p = case p.edited of
            Just pr -> lookup li (zip [0 ..] (viewProject pr).stack)
            Nothing -> Nothing
    w.openLayerPanel li >> pump 30
    forM_ [("name", Aeson.String "Sky"), ("opacity", Aeson.Number 0.5), ("opacity", Aeson.Number 0.4), ("blend", Aeson.String "multiply"), ("depth", Aeson.Number 3), ("shadow", Aeson.Bool True)] $ \(k, v) -> w.setInPanel k v >> pump 20
    p1 <- state
    check "the panel renames and restyles the layer" (fmap (\l -> (l.name, l.opacity, l.mixing, l.distance, isJust l.shadow)) (layerOf p1) == Just ("Sky", 0.4, Multiply, 3, True)) (show (fmap (\l -> (l.name, l.opacity, l.mixing, l.distance)) (layerOf p1)))
    check "a slider's drag is one undo step" (p1.undoSteps - p0.undoSteps == 5) (show (p1.undoSteps - p0.undoSteps))
    -- The camera: a slider keys the shot at this frame; dragging moves it, one undo step.
    keyed "c"
    w.setShotWith "zoom" 2 >> pump 30
    k1 <- state
    let
        keysOf p = maybe [] (\pr -> (viewProject pr).shots) p.edited
        near (ax, ay) (bx, by) = abs (ax - bx) < 0.01 && abs (ay - by) < 0.01
    check "a camera slider keys this frame" (map (\k -> (k.frame, k.zoom)) (keysOf k1) == [(k1.atFrame, 2)]) (show (map (\k -> (k.frame, k.zoom)) (keysOf k1), k1.atFrame))
    before <- state
    drag (960, 540) (1160, 640)
    k2 <- state
    check "dragging moves the shot, as one undo step" (map (near (1160, 640) . (.center)) (keysOf k2) == [True] && k2.undoSteps - before.undoSteps == 1) (show (map (.center) (keysOf k2), k2.undoSteps - before.undoSteps))
    snapshot w.canvasArea ("ui-snapshots" </> "09-camera.png")
    void (w.key "right" False False) >> pump 20
    w.setShotWith "zoom" 1.5 >> pump 30
    k3 <- state
    check "a change on another frame adds a second key, in order" (map (.frame) (keysOf k3) == [0, 1]) (show (map (.frame) (keysOf k3)))
    keyed "b"
    -- The menu bar's commands: a tool, undo once (not twice when the menu and the keys
    -- both hear ⌘Z), and back.
    w.command "tool-camera" >> pump 20
    m1 <- state
    check "the Tools menu picks a tool" (m1.toolName == "camera") (show m1.toolName)
    w.command "tool-brush" >> pump 20
    drag (300, 900) (600, 950)
    m2 <- state
    w.command "undo"
    void (w.key "z" True False)
    pump 30
    m3 <- state
    check "Undo from the menu and ⌘Z at once undo one step" (m2.undoSteps - m3.undoSteps == 1) (show (m2.undoSteps, m3.undoSteps))
    w.command "redo" >> pump 30
    m4 <- state
    check "Redo from the menu brings it back" (m4.undoSteps == m2.undoSteps) (show (m4.undoSteps, m2.undoSteps))
    -- Export, as the dialogs hand it over: a small MP4 of the project.
    let
        film = dir </> "film.mp4"
    case ffmpeg of
        Nothing -> putStrLn "skip the export: no ffmpeg on the PATH"
        Just _ -> do
            w.exportFile Export {format = H264, size = Size "test" 480 270, quality = Standard, fit = Contain, transparent = False} film
            e1 <- waitFor w 20000 (\p -> "Exported" `Text.isPrefixOf` p.note || "Not exported" `Text.isPrefixOf` p.note)
            bytes <- doesFileExist film >>= \here -> if here then getFileSize film else pure 0
            check "Export writes the film" ("Exported" `Text.isPrefixOf` e1.note && bytes > 1000) (Text.unpack e1.note <> " " <> show bytes)
    -- Saving: a moment after the last edit, the folder has what the window has.
    s13 <- waitFor w 6000 (\p -> not p.unsaved && p.note == "Saved")
    check "it saves" (not s13.unsaved && s13.note == "Saved") (Text.unpack s13.note)
    stored <- b.load pid
    check "the folder has what the window has" (either (const False) ((== s13.edited) . Just . fst) stored) (either Text.unpack (const "different") stored)
    snapshot whole (shots </> "04-window.png")
    Gtk.widgetDestroy w.window
    removeDirectoryRecursive dir
    n <- readIORef failures
    putStrLn (if n == 0 then "all UI checks pass; snapshots in " <> shots else show n <> " UI checks failed; snapshots in " <> shots)
    when (n > 0) exitFailure
