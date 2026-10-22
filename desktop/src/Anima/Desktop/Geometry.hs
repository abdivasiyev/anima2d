{- | Where items are, for the tools: whether a press touches one, and the box around a
selection. In the drawing's own space, as the items are stored.
-}
module Anima.Desktop.Geometry
    ( Box (..)
    , hits
    , itemBox
    , boxOf
    , topmostAt
    )
where

import Anima.Types.Project
import Data.Maybe (mapMaybe)

import Data.Text qualified as Text

data Box = Box {x0 :: Double, y0 :: Double, x1 :: Double, y1 :: Double}

-- | Whether a press at (x, y) of radius `r` touches the item (erasers are never hit).
hits :: ItemView -> Double -> Double -> Double -> Bool
hits item x y r = case item of
    EraseV _ -> False
    StrokeV s -> near [(a, b) | (a, b, _) <- s.points] (r + s.size / 2)
    ScatterV s -> near [(a, b) | (a, b, _) <- s.points] (r + s.size * 1.8)
    CurveV c -> near [c.start, c.control, c.end] (r + c.size / 2)
    ShapeV s -> inBox s.center s.width s.height s.rotation (r + s.size)
    FillV f -> any (inside (x, y)) f.polygons
    TextV t -> inBox t.center (textWidth t) (t.fontSize * 1.2 * lineCount t) t.rotation r
    ImageV i -> inBox i.center i.width i.height i.rotation r
    where
        near ps reach = case ps of
            [p] -> dist p <= reach
            _ -> any (\seg -> segDist seg <= reach) (zip ps (drop 1 ps))
        dist (a, b) = sqrt ((x - a) ** 2 + (y - b) ** 2)
        segDist ((ax, ay), (bx, by)) =
            let
                dx = bx - ax
                dy = by - ay
                l2 = dx * dx + dy * dy
                t = if l2 == 0 then 0 else max 0 (min 1 (((x - ax) * dx + (y - ay) * dy) / l2))
            in
                sqrt ((x - (ax + dx * t)) ** 2 + (y - (ay + dy * t)) ** 2)
        inBox (cx, cy) w h rot pad =
            let
                c = cos (-rot)
                s = sin (-rot)
                lx = (x - cx) * c - (y - cy) * s
                ly = (x - cx) * s + (y - cy) * c
            in
                abs lx <= w / 2 + pad && abs ly <= h / 2 + pad
        inside (px, py) poly =
            odd (length [() | ((ax, ay), (bx, by)) <- zip poly (drop 1 poly <> take 1 poly), (ay > py) /= (by > py), px < (bx - ax) * (py - ay) / (by - ay) + ax])

lineCount :: TextView -> Double
lineCount t = fromIntegral (max 1 (length (Text.splitOn "\n" t.text)))

-- | About half an em a character: near enough for picking and boxes.
textWidth :: TextView -> Double
textWidth t = t.fontSize * 0.58 * fromIntegral (maximum (1 : map Text.length (Text.splitOn "\n" t.text)))

-- | The topmost item under (x, y): line over colour, later over earlier.
topmostAt :: [ItemView] -> Double -> Double -> Double -> Maybe Int
topmostAt items x y r =
    case [i | pl <- [LinePlane, ColorPlane], (i, item) <- reverse (zip [0 ..] items), planeOf item == Just pl, hits item x y r] of
        i : _ -> Just i
        [] -> Nothing
    where
        planeOf = \case
            EraseV _ -> Nothing
            StrokeV s -> Just s.plane
            ScatterV s -> Just s.plane
            ShapeV s -> Just s.plane
            CurveV s -> Just s.plane
            FillV s -> Just s.plane
            TextV s -> Just s.plane
            ImageV s -> Just s.plane

-- | The box around an item, its line width included.
itemBox :: ItemView -> Maybe Box
itemBox = \case
    EraseV _ -> Nothing
    StrokeV s -> around [(a, b) | (a, b, _) <- s.points] (s.size / 2)
    ScatterV s -> around [(a, b) | (a, b, _) <- s.points] (s.size * 1.8)
    CurveV c -> around [c.start, c.control, c.end] (c.size / 2)
    ShapeV s -> around (corners s.center s.width s.height s.rotation) (s.size / 2)
    FillV f -> around (concat f.polygons) 0
    TextV t -> around (corners t.center (textWidth t) (t.fontSize * 1.2 * lineCount t) t.rotation) 0
    ImageV i -> around (corners i.center i.width i.height i.rotation) 0
    where
        around ps pad = case ps of
            [] -> Nothing
            _ -> Just Box {x0 = minimum (map fst ps) - pad, y0 = minimum (map snd ps) - pad, x1 = maximum (map fst ps) + pad, y1 = maximum (map snd ps) + pad}
        corners (cx, cy) w h r =
            [(cx + px * cos r - py * sin r, cy + px * sin r + py * cos r) | (px, py) <- [(-(w / 2), -(h / 2)), (w / 2, -(h / 2)), (w / 2, h / 2), (-(w / 2), h / 2)]]

-- | The box around several items.
boxOf :: [ItemView] -> Maybe Box
boxOf items = case mapMaybe itemBox items of
    [] -> Nothing
    bs -> Just Box {x0 = minimum (map (.x0) bs), y0 = minimum (map (.y0) bs), x1 = maximum (map (.x1) bs), y1 = maximum (map (.y1) bs)}
