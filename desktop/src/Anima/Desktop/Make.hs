{- | Items from what the pen did, made the one way anything enters a project: written as wire
JSON and parsed by the library's 'Item' parser, so the desktop cannot make an item the
browser, the server or an agent would refuse.
-}
module Anima.Desktop.Make
    ( Ink (..)
    , ShapeTool (..)
    , stroke
    , erase
    , scatterPath
    , shape
    , textAt
    , fillArea
    , seedFrom
    )
where

import Anima.Types.Parse (Parse (..))
import Anima.Types.Project (Item)
import Data.Aeson ((.=))
import Data.Text (Text)

import Data.Aeson qualified as Aeson

-- | How the brush draws: its colour, size and opacity.
data Ink = Ink {color :: Text, size :: Double, opacity :: Double}

data ShapeTool = LineShape | RectShape | EllipseShape | TriangleShape
    deriving stock (Bounded, Enum, Eq, Show)

points :: [(Double, Double, Double)] -> Aeson.Value
points ps = Aeson.toJSON [[x, y, max 0 (min 1 p)] | (x, y, p) <- ps]

clampSize :: Double -> Double
clampSize = max 0.5 . min 200

clampOpacity :: Double -> Double
clampOpacity = max 0.05 . min 1

-- | A freehand line; the paint brush goes under the line art, the others on it.
stroke :: Text -> Ink -> [(Double, Double, Double)] -> Either Text Item
stroke brush pen ps =
    parse
        ( Aeson.object
            [ "kind" .= ("stroke" :: Text)
            , "layer" .= (if brush == "paint" then "color" else "line" :: Text)
            , "brush" .= brush
            , "color" .= pen.color
            , "size" .= clampSize pen.size
            , "opacity" .= clampOpacity pen.opacity
            , "points" .= points ps
            ]
        )

-- | The pixel eraser's path.
erase :: Double -> [(Double, Double, Double)] -> Either Text Item
erase size ps = parse (Aeson.object ["kind" .= ("erase" :: Text), "size" .= clampSize size, "points" .= points ps])

-- | Stamps (grass, birds, …) along a path; `seed` fixes their randomness.
scatterPath :: Text -> Ink -> Int -> [(Double, Double, Double)] -> Either Text Item
scatterPath brush pen seed ps =
    parse
        ( Aeson.object
            [ "kind" .= ("scatter" :: Text)
            , "layer" .= ("line" :: Text)
            , "brush" .= brush
            , "color" .= pen.color
            , "size" .= clampSize pen.size
            , "opacity" .= clampOpacity pen.opacity
            , "seed" .= seed
            , "points" .= points ps
            ]
        )

{- | A shape dragged from `a` to `b`: a line from end to end, or a box, ellipse or triangle
filling the drag; `square` makes boxes square and circles round, and lines snap to 15°.
-}
shape :: ShapeTool -> Ink -> Bool -> Bool -> (Double, Double) -> (Double, Double) -> Either Text Item
shape tool pen filled square (ax, ay) (bx0, by0) =
    let
        (bx, by)
            | not square = (bx0, by0)
            | tool == LineShape =
                let
                    d = sqrt ((bx0 - ax) ** 2 + (by0 - ay) ** 2)
                    a = fromIntegral (round (atan2 (by0 - ay) (bx0 - ax) / (pi / 12)) :: Int) * (pi / 12)
                in
                    (ax + d * cos a, ay + d * sin a)
            | otherwise =
                let
                    k = max (abs (bx0 - ax)) (abs (by0 - ay))
                in
                    (ax + signum (bx0 - ax) * k, ay + signum (by0 - ay) * k)
        dx = bx - ax
        dy = by - ay
        (w, h, rot) = case tool of
            LineShape -> (sqrt (dx * dx + dy * dy), 0, atan2 dy dx)
            _ -> (abs dx, abs dy, 0)
        name = case tool of
            LineShape -> "line"
            RectShape -> "rect"
            EllipseShape -> "ellipse"
            TriangleShape -> "triangle" :: Text
    in
        parse
            ( Aeson.object
                [ "kind" .= ("shape" :: Text)
                , "layer" .= ("line" :: Text)
                , "shape" .= name
                , "color" .= pen.color
                , "size" .= clampSize pen.size
                , "opacity" .= clampOpacity pen.opacity
                , "fill" .= (if filled && tool /= LineShape then Aeson.toJSON pen.color else Aeson.Null)
                , "center" .= [ax + dx / 2, ay + dy / 2]
                , "width" .= w
                , "height" .= h
                , "rotation" .= rot
                ]
            )

-- | Text at a point, in a browser-safe font.
textAt :: Ink -> Text -> Double -> (Double, Double) -> Either Text Item
textAt pen body fontSize (x, y) =
    parse
        ( Aeson.object
            [ "kind" .= ("text" :: Text)
            , "layer" .= ("line" :: Text)
            , "text" .= body
            , "font" .= ("sans-serif" :: Text)
            , "style" .= ("normal" :: Text)
            , "size" .= max 4 (min 2000 fontSize)
            , "align" .= ("center" :: Text)
            , "color" .= pen.color
            , "opacity" .= clampOpacity pen.opacity
            , "center" .= [x, y]
            , "rotation" .= (0 :: Double)
            ]
        )

-- | Paint in the area a bucket click found (outlines, holes included), under the line art.
fillArea :: Ink -> [[(Double, Double)]] -> Either Text Item
fillArea ink polys =
    parse
        ( Aeson.object
            [ "kind" .= ("fill" :: Text)
            , "layer" .= ("color" :: Text)
            , "color" .= ink.color
            , "opacity" .= clampOpacity ink.opacity
            , "polygons" .= [[[x, y] | (x, y) <- poly] | poly <- polys]
            ]
        )

-- | A scatter seed from where and when the stroke began (any whole number works).
seedFrom :: Double -> Double -> Int -> Int
seedFrom x y t = abs (round (x * 7919 + y * 104729) + t * 31) `mod` 2147483647
