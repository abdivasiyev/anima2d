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
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import Data.Text qualified as Text
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Connector qualified as Connector
import GI.Gtk qualified as Gtk

import Anima.Desktop.Backend (Backend (..), localBackend)
import Anima.Desktop.Config (Connection (..))
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
    w.connectTo (Local dir)
    made <- createProject b "UI test"
    pid <- either (\e -> putStrLn ("could not make the project: " <> Text.unpack e) >> exitFailure) pure made
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
