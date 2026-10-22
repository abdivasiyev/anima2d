{- | A whole frame onto Cairo, as the browser editor composes it (@web/src/editor/output.ts@):
every layer at the frame, moved by its motion keys, faded, blended, clipped to the layer
below, with its shadow and rim light; lights drawn as light; and, through the camera,
parallax by depth, shake, depth of field and a tint over the frame.
-}
module Anima.Desktop.Scene
    ( Env (..)
    , Camera (..)
    , Cache
    , newCache
    , duration
    , celAt
    , cameraAt
    , placement
    , drawLayers
    , drawFrame
    , renderPng
    )
where

import Anima.Types.Project hiding (duration)
import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import GI.Cairo.Render.Matrix (Matrix (..))

import Data.Map.Strict qualified as Map
import GI.Cairo.Render qualified as C

import Anima.Desktop.Draw (Pictures, Rgb, drawCel, hexRgb)

-- | Flattened drawings, by layer and drawing index, for one version of a project.
newtype Cache = Cache (IORef (Map (Int, Int) C.Surface))

newCache :: IO Cache
newCache = Cache <$> newIORef Map.empty

data Env = Env {pictures :: Pictures, cache :: Cache, project :: ProjectView}

-- Time -----------------------------------------------------------------------------------

layerLength :: [CelView] -> Int
layerLength = sum . map (.hold)

-- | The timeline's length: its longest drawing layer.
duration :: ProjectView -> Int
duration p = maximum (1 : [layerLength cels | LayerView {kind = DrawingView _ cels} <- p.stack])

-- | The drawing showing at frame `t`; past its end a layer shows nothing, unless it holds its last.
celAt :: Bool -> [CelView] -> Int -> Maybe Int
celAt holdLast cels t = go 0 (zip [0 ..] cels)
    where
        go _ [] = if holdLast && not (null cels) then Just (length cels - 1) else Nothing
        go acc ((i, c) : rest) = if t < acc + c.hold then Just i else go (acc + c.hold) rest

-- Easing ---------------------------------------------------------------------------------

ease :: Ease -> Double -> Double
ease e u0 =
    let
        u = max 0 (min 1 u0)
    in
        case e of
            Linear -> u
            EaseIn -> u * u * u
            EaseOut -> 1 - (1 - u) ** 3
            EaseInOut -> if u < 0.5 then 4 * u * u * u else 1 - (-(2 * u) + 2) ** 3 / 2

shortTurn :: Double -> Double -> Double
shortTurn a b = atan2 (sin (b - a)) (cos (b - a))

-- | The pair of keys around `t` and how far between them, eased by the earlier; or the one held.
between :: (k -> Int) -> (k -> Ease) -> [k] -> Int -> Maybe (k, k, Double)
between frameOf easeOf keys t = case keys of
    [] -> Nothing
    first : _
        | t <= frameOf first -> Just (first, first, 0)
        | otherwise -> go keys
    where
        go = \case
            a : b : rest
                | t <= frameOf b -> Just (a, b, ease (easeOf a) (fromIntegral (t - frameOf a) / fromIntegral (max 1 (frameOf b - frameOf a))))
                | otherwise -> go (b : rest)
            [a] -> Just (a, a, 0)
            [] -> Nothing

-- Motion ---------------------------------------------------------------------------------

-- | A layer's transform at frame `t` (applied about its pivot), and its fade.
placement :: LayerView -> Int -> (Matrix, Double)
placement l t = case l.motion of
    Nothing -> (Matrix 1 0 0 1 0 0, 1)
    Just m -> case between (.frame) (.ease) m.keys t of
        Nothing -> (Matrix 1 0 0 1 0 0, 1)
        Just (a, b, u) ->
            let
                lerp lo hi = lo + (hi - lo) * u
                x = lerp (fst a.position) (fst b.position)
                y = lerp (snd a.position) (snd b.position)
                k = exp (lerp (log a.scale) (log b.scale))
                r = a.rotation + shortTurn a.rotation b.rotation * u
                fade = lerp a.opacity b.opacity
                (px, py) = m.pivot
                -- translate(x, y) · about(pivot, rotate · scale)
                mat = Matrix (k * cos r) (k * sin r) (-(k * sin r)) (k * cos r) (x + px - k * (cos r * px - sin r * py)) (y + py - k * (sin r * px + cos r * py))
            in
                (mat, fade)

-- Camera ---------------------------------------------------------------------------------

data Camera = Camera {cx :: Double, cy :: Double, zoom :: Double, rotation :: Double, shake :: Double, focus :: Double, aperture :: Double, tint :: Maybe Rgb, tintAmount :: Double}

-- | Where the camera is at frame `t`; Nothing when the project has none.
cameraAt :: ProjectView -> Int -> Maybe Camera
cameraAt p t = do
    (a, b, u) <- between (.frame) (.ease) p.shots t
    let
        lerp lo hi = lo + (hi - lo) * u
        amt k = if isJust k.tint then k.tintAmount else 0
        tintA = hexRgb <$> a.tint
        tintB = hexRgb <$> b.tint
        mix (r1, g1, b1) (r2, g2, b2) = (lerp r1 r2, lerp g1 g2, lerp b1 b2)
        tint = case (tintA, tintB) of
            (Just x, Just y) -> Just (mix x y)
            (x, y) -> x <|> y
    pure
        Camera
            { cx = lerp (fst a.center) (fst b.center)
            , cy = lerp (snd a.center) (snd b.center)
            , zoom = exp (lerp (log a.zoom) (log b.zoom))
            , rotation = a.rotation + shortTurn a.rotation b.rotation * u
            , shake = lerp a.shake b.shake
            , focus = lerp a.focus b.focus
            , aperture = lerp a.aperture b.aperture
            , tint
            , tintAmount = lerp (amt a) (amt b)
            }

-- | Handheld shake: a few sines at odd rates, the same every time it plays.
shakeAt :: Camera -> Int -> (Double, Double)
shakeAt cam frame
    | cam.shake == 0 = (0, 0)
    | otherwise =
        let
            t = fromIntegral frame
            x = sin (t * 2.13) + 0.6 * sin (t * 5.31 + 1.3) + 0.3 * sin (t * 11.7 + 2.1)
            y = sin (t * 2.71 + 0.7) + 0.6 * sin (t * 4.63 + 2.9) + 0.3 * sin (t * 13.1 + 0.4)
        in
            (cam.shake * x / 1.9, cam.shake * y / 1.9)

-- | Canvas → output for a layer at `depth`: farther layers travel and zoom less (parallax).
applyCamera :: Camera -> Double -> Double -> Double -> (Double, Double) -> C.Render ()
applyCamera cam w h depth (sx, sy) = do
    let
        x = w / 2 + (cam.cx + sx - w / 2) / depth
        y = h / 2 + (cam.cy + sy - h / 2) / depth
        z = cam.zoom ** (1 / depth)
    C.translate (w / 2) (h / 2)
    C.scale z z
    C.rotate (-cam.rotation)
    C.translate (-x) (-y)

-- Layers ---------------------------------------------------------------------------------

blendOp :: Blend -> C.Operator
blendOp = \case
    Normal -> C.OperatorOver
    Multiply -> C.OperatorMultiply
    Screen -> C.OperatorScreen
    Overlay -> C.OperatorOverlay
    Add -> C.OperatorAdd
    Darken -> C.OperatorDarken
    Lighten -> C.OperatorLighten

-- | A drawing flattened once, then reused.
celSurface :: Env -> Int -> Int -> [ItemView] -> C.Render C.Surface
celSurface env li ci items = do
    let
        Cache ref = env.cache
    known <- C.liftIO (readIORef ref)
    case Map.lookup (li, ci) known of
        Just s -> pure s
        Nothing -> C.liftIO $ do
            s <- C.createImageSurface C.FormatARGB32 env.project.width env.project.height
            C.renderWith s (drawCel env.pictures Nothing items)
            modifyIORef' ref (Map.insert (li, ci) s)
            pure s

-- | A light's gradient, over more than the canvas so it still covers it when moved.
drawLight :: LightView -> Double -> Double -> C.Render ()
drawLight l w h = do
    let
        (x, y) = l.center
        (r, g, b) = hexRgb l.color
        cover pat = do
            C.setSource pat
            C.rectangle (-w) (-h) (w * 3) (h * 3)
            C.fill
    if l.shape == RadialLight
        then C.withRadialPattern x y (min (l.radius * 0.999) (l.radius * (1 - l.softness))) x y l.radius $ \p -> do
            C.patternAddColorStopRGBA p 0 r g b l.intensity
            C.patternAddColorStopRGBA p 1 r g b 0
            cover p
        else do
            -- A band across the canvas, brightest on the line through the centre, fading along `angle`.
            let
                dx = cos l.angle * l.radius
                dy = sin l.angle * l.radius
                k = min 0.4995 ((1 - l.softness) / 2)
            C.withLinearPattern (x - dx) (y - dy) (x + dx) (y + dy) $ \p -> do
                C.patternAddColorStopRGBA p 0 r g b 0
                C.patternAddColorStopRGBA p (0.5 - k) r g b l.intensity
                C.patternAddColorStopRGBA p (0.5 + k) r g b l.intensity
                C.patternAddColorStopRGBA p 1 r g b 0
                cover p

-- | `src` painted softened by `px` canvas pixels: drawn small and stretched back (Cairo has no blur).
softPaint :: C.Surface -> Double -> Double -> Double -> Double -> C.Render ()
softPaint src w h px alpha
    | px < 0.5 = C.setSourceSurface src 0 0 >> C.paintWithAlpha alpha
    | otherwise = do
        let
            s = max (1 / 32) (min 1 (1.5 / px))
            tw = max 1 (ceiling (w * s))
            th = max 1 (ceiling (h * s))
        tiny <- C.liftIO (C.createImageSurface C.FormatARGB32 tw th)
        C.liftIO $ C.renderWith tiny $ do
            C.scale s s
            C.setSourceSurface src 0 0
            C.paint
        C.save
        C.scale (1 / s) (1 / s)
        C.setSourceSurface tiny 0 0
        pat <- C.getSource
        C.patternSetFilter pat C.FilterBilinear
        C.paintWithAlpha alpha
        C.restore

-- | The layer's picture with its shadow under and rim light over.
withEffects :: LayerView -> Double -> Double -> C.Render () -> C.Render ()
withEffects l w h content
    | not (isJust l.shadow || isJust l.rim) = content
    | otherwise = do
        pic <- surfaceOf content
        forM_ l.shadow $ \sh -> do
            sil <- silhouette pic (hexRgb sh.color) Nothing
            C.save
            uncurry C.translate sh.offset
            softPaint sil w h sh.blur sh.opacity
            C.restore
        C.setSourceSurface pic 0 0
        C.paint
        forM_ l.rim $ \rim -> when (rim.width > 0) $ do
            -- The edge facing the light: where the shape is, and is not once moved towards it.
            sil <- silhouette pic (hexRgb rim.color) (Just (-(cos rim.angle * rim.width), -(sin rim.angle * rim.width)))
            softPaint sil w h (min 4 (rim.width / 4)) rim.opacity
    where
        iw = ceiling w :: Int
        ih = ceiling h :: Int
        surfaceOf r = C.liftIO $ do
            s <- C.createImageSurface C.FormatARGB32 iw ih
            C.renderWith s r
            pure s
        silhouette pic (cr, cg, cb) cut = C.liftIO $ do
            s <- C.createImageSurface C.FormatARGB32 iw ih
            C.renderWith s $ do
                C.setSourceSurface pic 0 0
                C.paint
                forM_ cut $ \(dx, dy) -> do
                    C.setOperator C.OperatorDestOut
                    C.setSourceSurface pic dx dy
                    C.paint
                C.setOperator C.OperatorIn
                C.setSourceRGBA cr cg cb 1
                C.paint
            pure s

-- | Through the camera: where a layer at a depth goes, and how much it blurs.
data Through = Through {place :: Double -> C.Render (), blurOf :: Double -> Double}

{- | The layers `which` (indices, bottom to top) at frame `t`, onto the current target. A
clipped layer shows only where the nearest unclipped layer below it is; the base and the
layers clipped to it are composed apart, then blended in as one at the base's depth.
`override` draws one layer's content instead of its drawing (the edited drawing, live).
-}
drawLayers :: Env -> Int -> [Int] -> Maybe Through -> Maybe (Int, C.Render ()) -> C.Render ()
drawLayers env t which through override = go which
    where
        p = env.project
        w = fromIntegral p.width
        h = fromIntegral p.height
        layerAt i = lookup i (zip [0 ..] p.stack)
        paint i alpha = forM_ (layerAt i) $ \l -> do
            let
                (m, fade) = placement l t
            C.save
            C.transform m
            C.pushGroup
            case (override, l.kind) of
                (Just (o, draw), _) | o == i -> withEffects l w h draw
                (_, LightLayerView li) -> drawLight li w h
                (_, ReferenceView _ _) -> pure ()
                (_, DrawingView holdLast cels) -> case celAt holdLast cels t of
                    Just ci
                        | Just cel <- lookup ci (zip [0 ..] cels)
                        , not (null cel.items) -> withEffects l w h $ do
                            s <- celSurface env i ci cel.items
                            C.setSourceSurface s 0 0
                            C.paint
                    _ -> pure ()
            C.popGroupToSource
            C.paintWithAlpha (if alpha then l.opacity * fade else 1)
            C.restore
        go = \case
            [] -> pure ()
            i : rest -> case layerAt i of
                Nothing -> go rest
                Just l
                    | l.clipped -> go rest
                    | otherwise -> do
                        let
                            (clipRun, after) = span (maybe False (.clipped) . layerAt) rest
                            clips = filter (maybe False (.visible) . layerAt) clipRun
                        when l.visible $ do
                            let
                                blur = maybe 0 (\th -> th.blurOf l.distance) through
                            C.save
                            forM_ through (\th -> th.place l.distance)
                            if null clips && blur < 0.5
                                then do
                                    C.setOperator (blendOp l.mixing)
                                    paint i True
                                else do
                                    group <- C.liftIO (C.createImageSurface C.FormatARGB32 p.width p.height)
                                    C.liftIO $ C.renderWith group $ do
                                        paintInto i True
                                        forM_ clips $ \c -> forM_ (layerAt c) $ \cl -> do
                                            one <- C.liftIO (C.createImageSurface C.FormatARGB32 p.width p.height)
                                            mask <- C.liftIO (C.createImageSurface C.FormatARGB32 p.width p.height)
                                            C.liftIO (C.renderWith one (paintInto c True))
                                            C.liftIO (C.renderWith mask (paintInto i False))
                                            C.liftIO $ C.renderWith one $ do
                                                C.setOperator C.OperatorDestIn
                                                C.setSourceSurface mask 0 0
                                                C.paint
                                            C.setOperator (blendOp cl.mixing)
                                            C.setSourceSurface one 0 0
                                            C.paint
                                            C.setOperator C.OperatorOver
                                    C.setOperator (blendOp l.mixing)
                                    softPaint group w h blur 1
                            C.restore
                        go after
        paintInto = paint

{- | The finished frame at `t`, `outW` × `outH`: white paper (or none), every drawing and light
layer (references are for tracing only), through the camera when `camera` and there is one.
-}
drawFrame :: Env -> Int -> Int -> Int -> Bool -> Bool -> C.Render ()
drawFrame env t outW outH camera white = do
    let
        p = env.project
        w = fromIntegral p.width
        h = fromIntegral p.height
        ow = fromIntegral outW
        oh = fromIntegral outH
        k = min (ow / w) (oh / h)
        which = [i | (i, l) <- zip [0 ..] p.stack, not (isReference l)]
        isReference l = case l.kind of
            ReferenceView _ _ -> True
            _ -> False
    C.save
    when white $ C.setSourceRGB 1 1 1 >> C.paint
    case if camera then cameraAt p t else Nothing of
        Nothing -> do
            C.translate ((ow - w * k) / 2) ((oh - h * k) / 2)
            C.scale k k
            drawLayers env t which Nothing Nothing
        Just cam -> do
            C.translate (ow / 2) (oh / 2)
            C.scale k k
            C.translate (-(w / 2)) (-(h / 2))
            let
                shake = shakeAt cam t
            drawLayers env t which (Just Through {place = \d -> applyCamera cam w h d shake, blurOf = \d -> cam.aperture * abs (d - cam.focus)}) Nothing
            forM_ cam.tint $ \(r, g, b) -> when (cam.tintAmount > 0) $ do
                C.identityMatrix
                C.setOperator (if white then C.OperatorMultiply else C.OperatorAtop)
                C.setSourceRGBA r g b cam.tintAmount
                C.paint
    C.restore

-- | A frame of a project to a PNG file (for tests and previews).
renderPng :: Pictures -> ProjectView -> Int -> Int -> FilePath -> IO ()
renderPng pics p t outW path = do
    cache <- newCache
    let
        outH = max 2 (round (fromIntegral outW * fromIntegral p.height / fromIntegral p.width :: Double))
        env = Env {pictures = pics, cache, project = p}
    s <- C.createImageSurface C.FormatARGB32 outW outH
    C.renderWith s (drawFrame env t outW outH True True)
    C.surfaceWriteToPNG s path
