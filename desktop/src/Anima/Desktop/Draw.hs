{- | One drawing onto Cairo: every kind of item, as the browser editor draws it
(@web/src/editor/render.ts@). A drawing is two planes, colour under line; each item goes
to its own plane, and the eraser cuts both, at its place in the order.
-}
module Anima.Desktop.Draw
    ( Pictures
    , Rgb
    , hexRgb
    , drawCel
    , drawItems
    , drawItem
    , tracePolygon
    )
where

import Anima.Types.Project
import Control.Monad (forM_, unless, when)
import Data.Char (digitToInt, isHexDigit)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import GI.Cairo.Render qualified as C
import GI.Cairo.Render.Connector qualified as Connector
import GI.Pango qualified as Pango
import GI.PangoCairo qualified as PangoCairo

import Anima.Desktop.Random (Gen, mulberry32, next)

import Anima.Desktop.Freehand qualified as Freehand

-- | Pictures by asset id, loaded before drawing.
type Pictures = Map Text C.Surface

type Rgb = (Double, Double, Double)

-- | "#rrggbb" as 0–1 channels; anything else (never, after parsing) is black.
hexRgb :: Text -> Rgb
hexRgb t = case Text.unpack t of
    ['#', a, b, c, d, e, f] | all isHexDigit [a, b, c, d, e, f] -> (ch a b, ch c d, ch e f)
    _ -> (0, 0, 0)
    where
        ch x y = fromIntegral (digitToInt x * 16 + digitToInt y) / 255

-- | Lightened (amount > 0) or darkened (< 0), as the browser's `shade`.
shade :: Rgb -> Double -> Rgb
shade (r, g, b) amount = (f r, f g, f b)
    where
        f v =
            let
                byte = fromIntegral (round (v * 255) :: Int) :: Double
                out = if amount >= 0 then byte + (255 - byte) * amount else byte * (1 + amount)
            in
                fromIntegral (round out :: Int) / 255

setRgb :: Rgb -> Double -> C.Render ()
setRgb (r, g, b) = C.setSourceRGBA r g b

-- | A drawing's two planes onto the current surface; `tint` draws everything in one colour (onion skins).
drawCel :: Pictures -> Maybe Rgb -> [ItemView] -> C.Render ()
drawCel pics tint items = forM_ [ColorPlane, LinePlane] $ \pl -> do
    C.pushGroup
    drawItems pics tint pl items
    C.popGroupToSource
    C.paint

-- | The items that belong on one plane (erasers on both), in order.
drawItems :: Pictures -> Maybe Rgb -> Plane -> [ItemView] -> C.Render ()
drawItems pics tint pl = mapM_ (\i -> when (onPlane pl i) (drawItem pics tint i))

onPlane :: Plane -> ItemView -> Bool
onPlane pl = \case
    EraseV _ -> True
    StrokeV s -> s.plane == pl
    ScatterV s -> s.plane == pl
    ShapeV s -> s.plane == pl
    CurveV s -> s.plane == pl
    FillV s -> s.plane == pl
    TextV s -> s.plane == pl
    ImageV s -> s.plane == pl

-- | The outline the browser uses for each brush (perfect-freehand settings).
brushOptions :: Brush -> Double -> Freehand.Options
brushOptions b size = case b of
    Pen -> Freehand.Options {size = size, thinning = 0.62, smoothing = 0.55, streamline = 0.2}
    Pencil -> Freehand.Options {size = size * 0.7, thinning = 0.35, smoothing = 0.4, streamline = 0.15}
    Marker -> Freehand.Options {size = size, thinning = 0, smoothing = 0.6, streamline = 0.25}
    Paint -> Freehand.Options {size = size, thinning = 0.2, smoothing = 0.65, streamline = 0.3}

eraseOptions :: Double -> Freehand.Options
eraseOptions size = Freehand.Options {size = size, thinning = 0.2, smoothing = 0.5, streamline = 0.2}

-- | Quadratic curves through the midpoints: a smooth edge without extra points.
tracePolygon :: [(Double, Double)] -> C.Render ()
tracePolygon = \case
    [] -> pure ()
    first@(fx, fy) : rest -> do
        C.moveTo fx fy
        let
            go (cx, cy) = \case
                [] -> pure ()
                (px, py) : more -> do
                    let
                        (nx, ny) = case more of
                            n : _ -> n
                            [] -> first
                        end = ((px + nx) / 2, (py + ny) / 2)
                    quad (cx, cy) (px, py) end
                    go end more
        go first rest
        C.closePath

-- | A quadratic Bézier from `p0` as a cubic (Cairo has no quadratic).
quad :: (Double, Double) -> (Double, Double) -> (Double, Double) -> C.Render ()
quad (x0, y0) (qx, qy) (x2, y2) =
    C.curveTo (x0 + 2 / 3 * (qx - x0)) (y0 + 2 / 3 * (qy - y0)) (x2 + 2 / 3 * (qx - x2)) (y2 + 2 / 3 * (qy - y2)) x2 y2

drawItem :: Pictures -> Maybe Rgb -> ItemView -> C.Render ()
drawItem pics tint item = do
    C.save
    case item of
        StrokeV s -> do
            let
                poly = Freehand.outline (brushOptions s.brush s.size) s.points
                col = fromMaybe (hexRgb s.color) tint
            C.newPath
            if s.brush == Pencil && isNothing tint
                then pencil poly col s.opacity
                else tracePolygon poly >> setRgb col s.opacity >> C.fill
        EraseV e -> do
            C.setOperator C.OperatorDestOut
            C.setSourceRGBA 0 0 0 1
            C.newPath
            tracePolygon (Freehand.outline (eraseOptions e.size) e.points)
            C.fill
        ScatterV s -> do
            C.pushGroup
            scatter s (fromMaybe (hexRgb s.color) tint) (isJust tint)
            C.popGroupToSource
            C.paintWithAlpha s.opacity
        ShapeV s -> do
            let
                col = fromMaybe (hexRgb s.color) tint
            C.setLineWidth s.size
            C.setLineJoin C.LineJoinRound
            C.setLineCap C.LineCapRound
            C.newPath
            if s.shape == Ellipse
                then do
                    C.save
                    uncurry C.translate s.center
                    C.rotate s.rotation
                    C.scale (max 1.0e-6 (s.width / 2)) (max 1.0e-6 (s.height / 2))
                    C.arc 0 0 1 0 (2 * pi)
                    C.restore
                else do
                    case shapeCorners s of
                        (x, y) : rest -> C.moveTo x y >> mapM_ (uncurry C.lineTo) rest
                        [] -> pure ()
                    unless (s.shape == Line) C.closePath
            C.pushGroup
            case s.fill of
                Just f | s.shape /= Line -> setRgb (fromMaybe (hexRgb f) tint) 1 >> C.fillPreserve
                _ -> pure ()
            setRgb col 1
            C.stroke
            C.popGroupToSource
            C.paintWithAlpha s.opacity
        CurveV c -> do
            setRgb (fromMaybe (hexRgb c.color) tint) c.opacity
            C.setLineWidth c.size
            C.setLineCap C.LineCapRound
            C.newPath
            uncurry C.moveTo c.start
            quad c.start c.control c.end
            C.stroke
        FillV f -> do
            setRgb (fromMaybe (hexRgb f.color) tint) f.opacity
            C.setFillRule C.FillRuleEvenOdd
            C.newPath
            forM_ f.polygons $ \case
                (x, y) : rest -> C.moveTo x y >> mapM_ (uncurry C.lineTo) rest >> C.closePath
                [] -> pure ()
            C.fill
        TextV t -> textBlock t (fromMaybe (hexRgb t.color) tint)
        ImageV i -> picture pics tint i
    C.restore

-- | A shape's outline points: a line's two ends, a box's corners, a triangle's three.
shapeCorners :: ShapeView -> [(Double, Double)]
shapeCorners s =
    let
        hw = s.width / 2
        hh = s.height / 2
        local = case s.shape of
            Line -> [(-hw, 0), (hw, 0)]
            Rect -> [(-hw, -hh), (hw, -hh), (hw, hh), (-hw, hh)]
            Triangle -> [(0, -hh), (hw, hh), (-hw, hh)]
            Ellipse -> []
        c = cos s.rotation
        n = sin s.rotation
        (cx, cy) = s.center
    in
        [(cx + x * c - y * n, cy + x * n + y * c) | (x, y) <- local]

-- Pencil -------------------------------------------------------------------------------

-- | The pencil: the stroke filled, with speckles cut out of it on its own group.
pencil :: [(Double, Double)] -> Rgb -> Double -> C.Render ()
pencil poly col alpha = do
    C.pushGroup
    tracePolygon poly
    setRgb col alpha
    C.fill
    grain <- C.liftIO grainSurface
    C.setOperator C.OperatorDestOut
    C.setSourceSurface grain 0 0
    pat <- C.getSource
    C.patternSetExtend pat C.ExtendRepeat
    tracePolygon poly
    C.fill
    C.popGroupToSource
    C.paint

-- | 96×96 speckles from seed 7, as the browser's grain.
grainSurface :: IO C.Surface
grainSurface = do
    s <- C.createImageSurface C.FormatARGB32 96 96
    C.renderWith s $ go (1400 :: Int) (mulberry32 7)
    pure s
    where
        go 0 _ = pure ()
        go k g0 = do
            let
                (a, g1) = next g0
                (x, g2) = next g1
                (y, g3) = next g2
                alpha = fromIntegral (round ((0.15 + a * 0.5) * 100) :: Int) / 100
            C.setSourceRGBA 0 0 0 alpha
            C.rectangle (fromIntegral (floor (x * 96) :: Int)) (fromIntegral (floor (y * 96) :: Int)) 1 1
            C.fill
            go (k - 1) g3

-- Scatter ------------------------------------------------------------------------------

spacing :: Scatter -> Double
spacing = \case
    Grass -> 0.32
    Birds -> 2.4
    Stars -> 1.3
    Leaves -> 0.95
    Flowers -> 1.5
    Spray -> 0.18

-- | Stamps along the path, evenly spaced, every random draw from the item's seed.
scatter :: ScatterView -> Rgb -> Bool -> C.Render ()
scatter s col tinted = do
    let
        base = s.size * 1.6
        step = max 1 (base * spacing s.brush)
        lift = if s.brush == Birds then 1.6 else 0.35
        stamp g (x, y, pr) = do
            let
                (r1, g1) = next g
                (r2, g2) = next g1
                (r3, g3) = next g2
                sz = base * (0.55 + pr * 0.65) * (0.7 + r1 * 0.5)
                jx = (r2 - 0.5) * sz * 0.5
                jy = (r3 - 0.5) * sz * lift
            C.save
            C.translate (x + jx) (y + jy)
            g4 <- stampOf s.brush sz g3 col tinted
            C.restore
            pure g4
        walk g _ [] = pure g
        walk g (prev, carry) (p : rest) = do
            let
                (px, py, pp) = prev
                (qx, qy, qp) = p
                dx = qx - px
                dy = qy - py
                d = sqrt (dx * dx + dy * dy)
                ts = takeWhile (<= d) (iterate (+ step) (step - carry))
            g' <- foldl (\mg t -> mg >>= \gg -> let k = if d == 0 then 0 else t / d in stamp gg (px + dx * k, py + dy * k, pp + (qp - pp) * k)) (pure g) ts
            let
                lastT = if null ts then step - carry - step else last ts
                carry' = d - lastT
            walk g' (p, carry') rest
    case s.points of
        first : rest -> do
            g <- stamp (mulberry32 s.seed) first
            _ <- walk g (first, 0) rest
            pure ()
        [] -> pure ()

fillWith :: Rgb -> C.Render ()
fillWith c = setRgb c 1 >> C.fill

-- | One stamp at the origin, `s` across; returns the generator after its draws.
stampOf :: Scatter -> Double -> Gen -> Rgb -> Bool -> C.Render Gen
stampOf kind s g0 col tinted = case kind of
    Grass -> do
        let
            (r0, g1) = next g0
            blades = 3 + floor (r0 * 3) :: Int
            blade g _ = do
                let
                    (a, ga) = next g
                    (b, gb) = next ga
                    (c, gc) = next gb
                    (d, gd) = next gc
                    (e, ge) = next gd
                    bx = (a - 0.5) * 0.6 * s
                    h = s * (0.8 + b * 0.9)
                    lean = (c - 0.5) * 0.9 * s
                    w = s * (0.06 + d * 0.05)
                    tone = (e - 0.5) * 0.3
                C.newPath
                C.moveTo (bx - w) 0
                quad (bx - w, 0) (bx - w * 0.2 + lean * 0.25, -(h * 0.55)) (bx + lean, -h)
                quad (bx + lean, -h) (bx + w * 0.6 + lean * 0.25, -(h * 0.55)) (bx + w, 0)
                C.closePath
                fillWith (if tinted then col else shade col tone)
                pure ge
        foldl (\mg i -> mg >>= \g -> blade g i) (pure g1) [1 .. blades]
    Birds -> do
        let
            (a, g1) = next g0
            (b, g2) = next g1
            (c, g3) = next g2
            flap = 0.35 + a * 0.6
            tone = (c - 0.5) * 0.2
        C.rotate ((b - 0.5) * 0.5)
        setRgb (if tinted then col else shade col tone) 1
        C.setLineWidth (max 1 (s * 0.1))
        C.setLineCap C.LineCapRound
        C.setLineJoin C.LineJoinRound
        C.newPath
        C.moveTo (-(s * 0.6)) (-(s * 0.1 * flap))
        quad (-(s * 0.6), -(s * 0.1 * flap)) (-(s * 0.3), -(s * 0.45 * flap)) (0, 0)
        quad (0, 0) (s * 0.3, -(s * 0.45 * flap)) (s * 0.6, -(s * 0.1 * flap))
        C.stroke
        pure g3
    Stars -> do
        let
            (t, g1) = next g0
            tone = (t - 0.5) * 0.3
            colour = if tinted then col else shade col tone
            (dotP, g2) = next g1
        if dotP < 0.35
            then do
                let
                    (r, g3) = next g2
                C.newPath
                C.arc 0 0 (s * (0.05 + r * 0.06)) 0 (2 * pi)
                fillWith colour
                pure g3
            else do
                let
                    (turn, g3) = next g2
                    (o, g4) = next g3
                    outer = s * (0.25 + o * 0.25)
                    inner = outer * 0.42
                C.rotate (turn * pi)
                C.newPath
                forM_ [0 .. 9 :: Int] $ \i -> do
                    let
                        r = if even i then outer else inner
                        a = fromIntegral i * pi / 5 - pi / 2
                    C.lineTo (cos a * r) (sin a * r)
                C.closePath
                fillWith colour
                pure g4
    Leaves -> do
        let
            (turn, g1) = next g0
            (lr, g2) = next g1
            (t, g3) = next g2
            l = s * (0.45 + lr * 0.3)
            tone = (t - 0.5) * 0.35
        C.rotate (turn * pi * 2)
        C.newPath
        C.moveTo 0 (-l)
        quad (0, -l) (l * 0.75, 0) (0, l)
        quad (0, l) (-(l * 0.75), 0) (0, -l)
        fillWith (if tinted then col else shade col tone)
        setRgb (if tinted then col else shade col (-0.3)) 1
        C.setLineWidth (max 0.5 (l * 0.06))
        C.newPath
        C.moveTo 0 (-(l * 0.85))
        C.lineTo 0 (l * 0.85)
        C.stroke
        pure g3
    Flowers -> do
        let
            (turn, g1) = next g0
            (rr, g2) = next g1
            (t, g3) = next g2
            r = s * (0.18 + rr * 0.1)
            tone = (t - 0.5) * 0.3
        C.rotate (turn * pi)
        forM_ [0 .. 4 :: Int] $ \i -> do
            let
                a = fromIntegral i * pi * 2 / 5
            C.newPath
            C.arc (cos a * r * 1.15) (sin a * r * 1.15) r 0 (2 * pi)
            fillWith (if tinted then col else shade col tone)
        C.newPath
        C.arc 0 0 (r * 0.75) 0 (2 * pi)
        fillWith (if tinted then col else hexRgb "#f3c843")
        pure g3
    Spray -> do
        let
            dotAt g _ = do
                let
                    (a, ga) = next g
                    (d, gd) = next ga
                    (r, gr) = next gd
                    ang = a * pi * 2
                    dist = sqrt d * s * 0.6
                C.newPath
                C.arc (cos ang * dist) (sin ang * dist) (max 0.4 (s * (0.02 + r * 0.035))) 0 (2 * pi)
                fillWith col
                pure gr
        foldl (\mg i -> mg >>= \g -> dotAt g i) (pure g0) [1 .. 7 :: Int]

-- Text and pictures --------------------------------------------------------------------

lineHeight :: Double
lineHeight = 1.2

-- | Lines of text, centred on the item's centre, turned, in its font (Pango).
textBlock :: TextView -> Rgb -> C.Render ()
textBlock t col = do
    ctx <- Connector.getContext
    uncurry C.translate t.center
    C.rotate t.rotation
    let
        lns = Text.splitOn "\n" t.text
        lh = t.fontSize * lineHeight
    desc <- Pango.fontDescriptionFromString (fontName t)
    measured <-
        C.liftIO
            $ mapM
                ( \ln -> do
                    layout <- PangoCairo.createLayout ctx
                    Pango.layoutSetFontDescription layout (Just desc)
                    Pango.layoutSetText layout ln (-1)
                    (_, logical) <- Pango.layoutGetPixelExtents layout
                    w <- Pango.getRectangleWidth logical
                    h <- Pango.getRectangleHeight logical
                    pure (layout, fromIntegral w, fromIntegral h)
                )
                lns
    let
        blockW = maximum (1 : [w | (_, w, _) <- measured])
        blockH = fromIntegral (length lns) * lh
    setRgb col t.opacity
    forM_ (zip [0 :: Int ..] measured) $ \(i, (layout, w, h)) -> do
        let
            x = case t.align of
                AlignLeft -> -(blockW / 2)
                AlignCenter -> -(w / 2)
                AlignRight -> blockW / 2 - w
            mid = -(blockH / 2) + lh * (fromIntegral i + 0.5)
        C.moveTo x (mid - h / 2)
        C.liftIO (PangoCairo.showLayout ctx layout)

-- | A Pango font name: family, then style, then the size in pixels.
fontName :: TextView -> Text
fontName t = t.font <> styleWords <> " " <> Text.pack (show (max 1 (round t.fontSize :: Int))) <> "px"
    where
        styleWords = case t.style of
            Regular -> ""
            Bold -> " Bold"
            Italic -> " Italic"
            BoldItalic -> " Bold Italic"

-- | A picture, centred, turned and mirrored; its outline until it has loaded.
picture :: Pictures -> Maybe Rgb -> ImageView -> C.Render ()
picture pics tint i = do
    uncurry C.translate i.center
    C.rotate i.rotation
    when i.mirrored (C.scale (-1) 1)
    case (Map.lookup i.asset pics, tint) of
        (Just surface, Nothing) -> do
            w <- C.imageSurfaceGetWidth surface
            h <- C.imageSurfaceGetHeight surface
            C.translate (-(i.width / 2)) (-(i.height / 2))
            C.scale (i.width / max 1 (fromIntegral w)) (i.height / max 1 (fromIntegral h))
            C.setSourceSurface surface 0 0
            C.paintWithAlpha i.opacity
        _ -> do
            C.rectangle (-(i.width / 2)) (-(i.height / 2)) i.width i.height
            setRgb (fromMaybe (0.56, 0.56, 0.59) tint) (0.12 * i.opacity)
            C.fillPreserve
            setRgb (fromMaybe (0.56, 0.56, 0.59) tint) i.opacity
            C.setLineWidth 2
            C.stroke
