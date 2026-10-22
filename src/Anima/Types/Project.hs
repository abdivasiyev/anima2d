{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- | An animation project: a canvas, a frame rate and the drawings on it.

Every type here is built by 'parse' and exported abstractly. The browser editor
(@web/src/domain/project.ts@) parses the same JSON into its own branded types, rule for
rule and message for message: both suites run the cases in @fixtures/@, so the two sides
agree on what a valid project is and on what they say when it is not.

Format 2 (written): each drawing is an ordered list of items on two layers; colour items
render under line items. Format 1 (read only): pen and eraser strokes, upgraded on parse.

Parsing reads 'Value's field by field, in wire order, the way the editor does: a missing
field reads as @null@ and is refused by that field's own parser, so the first rejection
and its words are the same on both sides.
-}
module Anima.Types.Project
    ( -- * Identity
      ProjectId
    , ProjectName

      -- * Settings
    , Fps
    , fpsValue
    , fpsFrom
    , Width
    , Height
    , Canvas
    , canvasFrom

      -- * Drawings
    , Item

      -- * Project
    , Project
    , projectId
    , duration
    , newProject
    , pickLayer
    , layersSummary
    , drawingAt
    , addItemsAt
    , replaceItemsAt
    , insertDrawing
    , addLayer
    , AssetId
    , Revision
    , firstRevision
    , nextRevision
    , ProjectSummary
    , summarize
    , summaryId
    , summaryName
    , summaryRevision
    , RawCanvas (..)
    , RawSummary (..)

      -- * Limits
    , maxFrames
    , maxItemsPerFrame
    , maxPointsPerItem
    )
where

import Control.Monad (unless, when, (>=>))
import Data.Aeson (FromJSON, ToJSON, Value (..), object, (.=))
import Data.Char (isAsciiLower, isAsciiUpper, isControl, isDigit, isHexDigit, ord, toLower)
import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isJust)
import Data.Scientific (fromFloatDigits, isInteger, toBoundedInteger, toRealFloat)
import Data.Text (Text)
import Data.Traversable (mapAccumL)
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Vector qualified as Vector

import Anima.Types.Parse (Parse (..), Parsed (..))

maxFrames :: Int
maxFrames = 10000

maxItemsPerFrame :: Int
maxItemsPerFrame = 5000

maxPointsPerItem :: Int
maxPointsPerItem = 20000

maxPolygonsPerFill :: Int
maxPolygonsPerFill = 500

maxPointsPerFill :: Int
maxPointsPerFill = 50000

maxCoordinate :: Double
maxCoordinate = 100000

maxExtent :: Int
maxExtent = 200000

maxSeed :: Int
maxSeed = 2147483647

-- Reading values ------------------------------------------------------------------------

type Fields = KeyMap.KeyMap Value

asObject :: Value -> Either Text Fields
asObject = \case
    Object o -> Right o
    _ -> Left "expected an object"

asArray :: Value -> Either Text [Value]
asArray = \case
    Array xs -> Right (Vector.toList xs)
    _ -> Left "expected an array"

asNumber :: Value -> Either Text Double
asNumber = \case
    Number n | finite (toRealFloat n) -> Right (toRealFloat n)
    _ -> Left "expected a number"

asInteger :: Value -> Either Text Int
asInteger = \case
    Number n | isInteger n, Just i <- toBoundedInteger n -> Right i
    _ -> Left "expected a whole number"

asText :: Value -> Either Text Text
asText = \case
    String t -> Right t
    _ -> Left "expected text"

-- | The field, @null@ when missing, through its parser; rejections name the field.
field :: Text -> (Value -> Either Text a) -> Fields -> Either Text a
field name p o = at name (p (fromMaybe Null (KeyMap.lookup (Key.fromText name) o)))

-- | Prefix a rejection with where it happened; the path is ours, never the input.
at :: Text -> Either Text a -> Either Text a
at here = either (Left . ((here <> ": ") <>)) Right

-- | Parse each element, naming the index of the first that fails.
indexed :: (a -> Either Text b) -> [a] -> Either Text [b]
indexed f = traverse (\(i, x) -> at (tshow i) (f x)) . snd . mapAccumL (\i x -> (i + 1, (i :: Int, x))) 0

bounded :: Text -> Int -> Int -> [a] -> Either Text [a]
bounded what lo hi xs
    | n < lo || n > hi = Left ("expected " <> tshow lo <> " to " <> tshow hi <> " " <> what)
    | otherwise = Right xs
    where
        n = length xs

-- | One of a closed vocabulary, by its wire name.
oneOf :: (Bounded a, Enum a) => (a -> Text) -> Text -> Value -> Either Text a
oneOf name expected = \case
    String t | (x : _) <- filter ((== t) . name) [minBound .. maxBound] -> Right x
    _ -> Left expected

nonEmpty :: Text -> [a] -> Either Text (NonEmpty a)
nonEmpty expected = maybe (Left expected) Right . NonEmpty.nonEmpty

ranged :: (Double -> a) -> Double -> Double -> Text -> Value -> Either Text a
ranged wrap lo hi expected = asNumber >=> \n -> if n >= lo && n <= hi then Right (wrap n) else Left expected

finite :: Double -> Bool
finite v = not (isNaN v || isInfinite v)

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

-- | Length as the editor counts it: UTF-16 code units.
utf16Length :: Text -> Int
utf16Length = Text.foldl' (\n c -> n + if ord c > 0xFFFF then 2 else 1) 0

-- | The shortest decimal that reads back as this double, as JavaScript writes it.
number :: Double -> Value
number = Number . fromFloatDigits

list :: [Value] -> Value
list = Array . Vector.fromList

-- Identity ------------------------------------------------------------------------------

-- | 1 to 64 of @a–z@, @0–9@ and @-@: a UUID in practice, and a safe file name.
newtype ProjectId = ProjectId Text
    deriving stock (Eq, Ord, Show)
    deriving (FromJSON, ToJSON) via Parsed ProjectId
    deriving (FromHttpApiData, ToHttpApiData) via Parsed ProjectId

instance Parse ProjectId where
    type Raw ProjectId = Text
    parse raw
        | Text.null raw || Text.length raw > 64 = Left "expected a project id of 1 to 64 characters"
        | Text.all idChar raw = Right (ProjectId raw)
        | otherwise = Left "expected a project id of a-z, 0-9 and '-'"
        where
            idChar c = isAsciiLower c || isDigit c || c == '-'
    unparse (ProjectId raw) = raw

-- | 1 to 80 characters (as the editor counts them), trimmed, no control characters.
newtype ProjectName = ProjectName Text
    deriving stock (Eq, Ord, Show)
    deriving (FromJSON, ToJSON) via Parsed ProjectName

instance Parse ProjectName where
    type Raw ProjectName = Text
    parse raw
        | Text.null trimmed || utf16Length trimmed > 80 = Left "expected a project name of 1 to 80 characters"
        | Text.any isControl trimmed = Left "expected a project name without control characters"
        | otherwise = Right (ProjectName trimmed)
        where
            trimmed = Text.strip raw
    unparse (ProjectName raw) = raw

-- Settings ------------------------------------------------------------------------------

data Fps = Fps12 | Fps24 | Fps25 | Fps30 | Fps48 | Fps50 | Fps60
    deriving stock (Bounded, Enum, Eq, Ord, Show)

fpsValue :: Fps -> Int
fpsValue = \case
    Fps12 -> 12
    Fps24 -> 24
    Fps25 -> 25
    Fps30 -> 30
    Fps48 -> 48
    Fps50 -> 50
    Fps60 -> 60

fpsFrom :: Value -> Either Text Fps
fpsFrom v = case [f | Number n <- [v], f <- [minBound .. maxBound], fromIntegral (fpsValue f) == n] of
    (f : _) -> Right f
    [] -> Left "expected a frame rate of 12, 24, 25, 30, 48, 50 or 60"

-- | Even (encoders refuse odd sizes), 64 to 7680. Two types, so the two never swap.
newtype Width = Width Int
    deriving stock (Eq, Ord, Show)

newtype Height = Height Int
    deriving stock (Eq, Ord, Show)

dimension :: Text -> Value -> Either Text Int
dimension what =
    asInteger >=> \n ->
        if
            | n < 64 || n > 7680 -> Left ("expected a " <> what <> " from 64 to 7680 pixels")
            | odd n -> Left ("expected an even " <> what)
            | otherwise -> Right n

data Canvas = Canvas Width Height
    deriving stock (Eq, Show)

canvasFrom :: Value -> Either Text Canvas
canvasFrom = asObject >=> \o -> Canvas <$> field "width" (fmap Width . dimension "width") o <*> field "height" (fmap Height . dimension "height") o

canvasValue :: Canvas -> Value
canvasValue (Canvas (Width w) (Height h)) = object ["width" .= w, "height" .= h]

-- | The summary file's wire shape (summaries are the server's own files).
data RawCanvas = RawCanvas {width :: Int, height :: Int}
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

-- Values items are made of --------------------------------------------------------------

-- | Which plane of a drawing an item is on: colour renders under line, always. (On the wire: "layer".)
data Plane = LinePlane | ColorPlane
    deriving stock (Bounded, Enum, Eq, Ord, Show)

planeName :: Plane -> Text
planeName = \case
    LinePlane -> "line"
    ColorPlane -> "color"

planeFrom :: Value -> Either Text Plane
planeFrom = oneOf planeName "expected a layer of \"line\" or \"color\""

data Brush = Pen | Pencil | Marker | Paint
    deriving stock (Bounded, Enum, Eq, Ord, Show)

brushName :: Brush -> Text
brushName = Text.toLower . tshow

data Scatter = Grass | Birds | Stars | Leaves | Flowers | Spray
    deriving stock (Bounded, Enum, Eq, Ord, Show)

scatterName :: Scatter -> Text
scatterName = Text.toLower . tshow

data ShapeKind = Line | Rect | Ellipse | Triangle
    deriving stock (Bounded, Enum, Eq, Ord, Show)

shapeName :: ShapeKind -> Text
shapeName = Text.toLower . tshow

-- | sRGB as @#rrggbb@, kept lower-case.
newtype Color = Color Text
    deriving stock (Eq, Ord, Show)

colorFrom :: Value -> Either Text Color
colorFrom =
    asText >=> \raw -> case Text.uncons raw of
        Just ('#', hex) | Text.length hex == 6 && Text.all isHexDigit hex -> Right (Color (Text.map toLower raw))
        _ -> Left "expected a colour as #rrggbb"

colorValue :: Color -> Value
colorValue (Color c) = String c

-- | A shape's fill: a colour, or none (@null@).
fillColorFrom :: Value -> Either Text (Maybe Color)
fillColorFrom = \case
    Null -> Right Nothing
    v -> either (const (Left "expected a colour as #rrggbb or null")) (Right . Just) (colorFrom v)

-- | Diameter in canvas pixels at full pressure, 0.5 to 200.
newtype BrushSize = BrushSize Double
    deriving stock (Eq, Ord, Show)

newtype Opacity = Opacity Double
    deriving stock (Eq, Ord, Show)

newtype Pressure = Pressure Double
    deriving stock (Eq, Ord, Show)

-- | Fixes a scatter stroke's randomness, so it renders the same every time.
newtype Seed = Seed Int
    deriving stock (Eq, Ord, Show)

-- | A shape's width or height, 0 to 200 000 pixels.
newtype Extent = Extent Double
    deriving stock (Eq, Ord, Show)

-- | Radians, -π to π.
newtype Angle = Angle Double
    deriving stock (Eq, Ord, Show)

brushSizeFrom :: Value -> Either Text BrushSize
brushSizeFrom = ranged BrushSize 0.5 200 "expected a brush size from 0.5 to 200"

opacityFrom :: Value -> Either Text Opacity
opacityFrom = ranged Opacity 0.05 1 "expected an opacity from 0.05 to 1"

pressureFrom :: Value -> Either Text Pressure
pressureFrom = ranged Pressure 0 1 "expected a pressure from 0 to 1"

extentFrom :: Value -> Either Text Extent
extentFrom = ranged Extent 0 (fromIntegral maxExtent) ("expected a size from 0 to " <> tshow maxExtent <> " pixels")

angleFrom :: Value -> Either Text Angle
angleFrom = ranged Angle (-pi) pi "expected an angle from -pi to pi"

seedFrom :: Value -> Either Text Seed
seedFrom = asInteger >=> \n -> if n >= 0 && n <= maxSeed then Right (Seed n) else Left ("expected a seed from 0 to " <> tshow maxSeed)

coordinate :: Value -> Maybe Double
coordinate v = case asNumber v of
    Right n | abs n <= maxCoordinate -> Just n
    _ -> Nothing

-- | A position in canvas pixels, at most 100 000 px from the origin.
data Vec = Vec Double Double
    deriving stock (Eq, Show)

vecFrom :: Value -> Either Text Vec
vecFrom = \case
    Array xs | [x, y] <- Vector.toList xs, Just px <- coordinate x, Just py <- coordinate y -> Right (Vec px py)
    _ -> Left "expected a position as [x, y] within 100000 pixels of the origin"

vecValue :: Vec -> Value
vecValue (Vec x y) = list [number x, number y]

-- | One pen sample: position and pressure.
data Point = Point Double Double Pressure
    deriving stock (Eq, Show)

pointFrom :: Value -> Either Text Point
pointFrom =
    asArray >=> \case
        [x, y, p] -> case (coordinate x, coordinate y) of
            (Just px, Just py) -> Point px py <$> pressureFrom p
            _ -> Left "expected coordinates within 100000 pixels of the origin"
        _ -> Left "expected a point as [x, y, pressure]"

pointsFrom :: Value -> Either Text (NonEmpty Point)
pointsFrom = asArray >=> bounded "points" 1 maxPointsPerItem >=> indexed pointFrom >=> nonEmpty "expected a point"

pointsValue :: NonEmpty Point -> Value
pointsValue = list . map (\(Point x y (Pressure p)) -> list [number x, number y, number p]) . NonEmpty.toList

curveFrom :: Value -> Either Text (Vec, Vec, Vec)
curveFrom = \case
    Array xs
        | [_, _, _] <- Vector.toList xs ->
            indexed vecFrom (Vector.toList xs) >>= \case
                [a, c, b] -> Right (a, c, b)
                _ -> Left curveExpected
    _ -> Left curveExpected
    where
        curveExpected = "expected a curve as [start, control, end]"

polygonsFrom :: Value -> Either Text (NonEmpty (NonEmpty Vec))
polygonsFrom =
    asArray
        >=> bounded "polygons" 1 maxPolygonsPerFill
        >=> indexed (asArray >=> bounded "points" 3 maxPointsPerItem >=> indexed vecFrom >=> nonEmpty "expected 3 points")
        >=> nonEmpty "expected a polygon"

-- Items ---------------------------------------------------------------------------------

-- | A freehand line: pen, pencil, marker, or the paint brush.
data StrokeItem = StrokeItem {layer :: Plane, brush :: Brush, color :: Color, size :: BrushSize, opacity :: Opacity, points :: NonEmpty Point}
    deriving stock (Eq, Show)

-- | The pixel eraser's path; it cuts both layers at its place in the order.
data EraseItem = EraseItem {size :: BrushSize, points :: NonEmpty Point}
    deriving stock (Eq, Show)

-- | A path stamped with grass, birds, stars and the like.
data ScatterItem = ScatterItem {layer :: Plane, brush :: Scatter, color :: Color, size :: BrushSize, opacity :: Opacity, seed :: Seed, points :: NonEmpty Point}
    deriving stock (Eq, Show)

-- | A line, rectangle, ellipse or triangle, by centre, size and turn.
data ShapeItem = ShapeItem
    { layer :: Plane
    , shape :: ShapeKind
    , color :: Color
    , size :: BrushSize
    , opacity :: Opacity
    , fill :: Maybe Color
    , center :: Vec
    , width :: Extent
    , height :: Extent
    , rotation :: Angle
    }
    deriving stock (Eq, Show)

-- | A quadratic Bézier: start, control point, end.
data CurveItem = CurveItem {layer :: Plane, color :: Color, size :: BrushSize, opacity :: Opacity, points :: (Vec, Vec, Vec)}
    deriving stock (Eq, Show)

-- | A paint-bucket fill: outlines filled even-odd, so holes stay open.
data FillItem = FillItem {layer :: Plane, color :: Color, opacity :: Opacity, polygons :: NonEmpty (NonEmpty Vec)}
    deriving stock (Eq, Show)

-- | Lines of text in a font, by the middle of the block, its turn and its alignment.
data TextItem = TextItem
    { layer :: Plane
    , text :: Text
    , font :: Text
    , style :: TextStyle
    , fontSize :: Double
    , align :: Align
    , color :: Color
    , opacity :: Opacity
    , center :: Vec
    , rotation :: Angle
    }
    deriving stock (Eq, Show)

data TextStyle = Regular | Bold | Italic | BoldItalic
    deriving stock (Bounded, Enum, Eq, Show)

textStyleName :: TextStyle -> Text
textStyleName = \case
    Regular -> "normal"
    Bold -> "bold"
    Italic -> "italic"
    BoldItalic -> "bold-italic"

data Align = AlignLeft | AlignCenter | AlignRight
    deriving stock (Bounded, Enum, Eq, Show)

alignName :: Align -> Text
alignName = \case
    AlignLeft -> "left"
    AlignCenter -> "center"
    AlignRight -> "right"

-- | A picture from the project's assets, by its middle, drawn size, turn and mirroring.
data ImageItem = ImageItem
    { layer :: Plane
    , asset :: AssetId
    , opacity :: Opacity
    , center :: Vec
    , width :: Extent
    , height :: Extent
    , rotation :: Angle
    , mirrored :: Bool
    }
    deriving stock (Eq, Show)

maxTextLength :: Int
maxTextLength = 2000

textBodyFrom :: Value -> Either Text Text
textBodyFrom =
    asText >=> \t ->
        if
            | Text.null (Text.strip t) || utf16Length t > maxTextLength -> Left ("expected text of 1 to " <> tshow maxTextLength <> " characters")
            | Text.any (\c -> isControl c && c /= '\n') t -> Left "expected text without control characters other than line breaks"
            | otherwise -> Right t

-- | A font family: one of the browser's (serif, sans-serif, ...) or one the project brings.
fontFamilyFrom :: Value -> Either Text Text
fontFamilyFrom =
    asText >=> \t ->
        if Text.length t >= 1 && Text.length t <= 64 && Text.all (\c -> isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` [' ', '-', '_']) t && Text.strip t == t
            then Right t
            else Left "expected a font family of 1 to 64 letters, digits, spaces, '-' or '_'"

fontSizeFrom :: Value -> Either Text Double
fontSizeFrom = ranged Prelude.id 4 2000 "expected a font size from 4 to 2000"

data Item
    = Stroke StrokeItem
    | Erase EraseItem
    | ScatterStroke ScatterItem
    | Shape ShapeItem
    | Curve CurveItem
    | Fill FillItem
    | TextBlock TextItem
    | Picture ImageItem
    deriving stock (Eq, Show)
    deriving (FromJSON, ToJSON) via Parsed Item

data Kind = KStroke | KErase | KScatter | KShape | KCurve | KFill | KText | KImage
    deriving stock (Bounded, Enum, Eq)

kindName :: Kind -> Text
kindName = \case
    KStroke -> "stroke"
    KErase -> "erase"
    KScatter -> "scatter"
    KShape -> "shape"
    KCurve -> "curve"
    KFill -> "fill"
    KText -> "text"
    KImage -> "image"

instance Parse Item where
    type Raw Item = Value
    parse v = do
        o <- asObject v
        kind <- field "kind" (oneOf kindName "expected an item kind of \"stroke\", \"erase\", \"scatter\", \"shape\", \"curve\", \"fill\", \"text\" or \"image\"") o
        item <- case kind of
            KStroke ->
                fmap Stroke
                    $ StrokeItem
                        <$> field "layer" planeFrom o
                        <*> field "brush" (oneOf brushName "expected a brush of \"pen\", \"pencil\", \"marker\" or \"paint\"") o
                        <*> field "color" colorFrom o
                        <*> field "size" brushSizeFrom o
                        <*> field "opacity" opacityFrom o
                        <*> field "points" pointsFrom o
            KErase -> fmap Erase $ EraseItem <$> field "size" brushSizeFrom o <*> field "points" pointsFrom o
            KScatter ->
                fmap ScatterStroke
                    $ ScatterItem
                        <$> field "layer" planeFrom o
                        <*> field "brush" (oneOf scatterName "expected a scatter brush of \"grass\", \"birds\", \"stars\", \"leaves\", \"flowers\" or \"spray\"") o
                        <*> field "color" colorFrom o
                        <*> field "size" brushSizeFrom o
                        <*> field "opacity" opacityFrom o
                        <*> field "seed" seedFrom o
                        <*> field "points" pointsFrom o
            KShape ->
                fmap Shape
                    $ ShapeItem
                        <$> field "layer" planeFrom o
                        <*> field "shape" (oneOf shapeName "expected a shape of \"line\", \"rect\", \"ellipse\" or \"triangle\"") o
                        <*> field "color" colorFrom o
                        <*> field "size" brushSizeFrom o
                        <*> field "opacity" opacityFrom o
                        <*> field "fill" fillColorFrom o
                        <*> field "center" vecFrom o
                        <*> field "width" extentFrom o
                        <*> field "height" extentFrom o
                        <*> field "rotation" angleFrom o
            KCurve ->
                fmap Curve
                    $ CurveItem
                        <$> field "layer" planeFrom o
                        <*> field "color" colorFrom o
                        <*> field "size" brushSizeFrom o
                        <*> field "opacity" opacityFrom o
                        <*> field "points" curveFrom o
            KFill ->
                fmap Fill
                    $ FillItem <$> field "layer" planeFrom o <*> field "color" colorFrom o <*> field "opacity" opacityFrom o <*> field "polygons" polygonsFrom o
            KText ->
                fmap TextBlock
                    $ TextItem
                        <$> field "layer" planeFrom o
                        <*> field "text" textBodyFrom o
                        <*> field "font" fontFamilyFrom o
                        <*> field "style" (oneOf textStyleName "expected a style of \"normal\", \"bold\", \"italic\" or \"bold-italic\"") o
                        <*> field "size" fontSizeFrom o
                        <*> field "align" (oneOf alignName "expected an alignment of \"left\", \"center\" or \"right\"") o
                        <*> field "color" colorFrom o
                        <*> field "opacity" opacityFrom o
                        <*> field "center" vecFrom o
                        <*> field "rotation" angleFrom o
            KImage ->
                fmap Picture
                    $ ImageItem
                        <$> field "layer" planeFrom o
                        <*> field "asset" assetIdFrom o
                        <*> field "opacity" opacityFrom o
                        <*> field "center" vecFrom o
                        <*> field "width" extentFrom o
                        <*> field "height" extentFrom o
                        <*> field "rotation" angleFrom o
                        <*> field "flip" boolFrom o
        checkFillBudget item
        pure item
    unparse = \case
        Stroke StrokeItem {..} ->
            object ["kind" .= kindName KStroke, "layer" .= planeName layer, "brush" .= brushName brush, "color" .= colorValue color, "size" .= sizeValue size, "opacity" .= opacityValue opacity, "points" .= pointsValue points]
        Erase EraseItem {..} -> object ["kind" .= kindName KErase, "size" .= sizeValue size, "points" .= pointsValue points]
        ScatterStroke ScatterItem {..} ->
            object
                [ "kind" .= kindName KScatter
                , "layer" .= planeName layer
                , "brush" .= scatterName brush
                , "color" .= colorValue color
                , "size" .= sizeValue size
                , "opacity" .= opacityValue opacity
                , "seed" .= let Seed s = seed in s
                , "points" .= pointsValue points
                ]
        Shape ShapeItem {..} ->
            object
                [ "kind" .= kindName KShape
                , "layer" .= planeName layer
                , "shape" .= shapeName shape
                , "color" .= colorValue color
                , "size" .= sizeValue size
                , "opacity" .= opacityValue opacity
                , "fill" .= maybe Null colorValue fill
                , "center" .= vecValue center
                , "width" .= let Extent e = width in number e
                , "height" .= let Extent e = height in number e
                , "rotation" .= let Angle a = rotation in number a
                ]
        Curve CurveItem {..} ->
            let
                (a, c, b) = points
            in
                object ["kind" .= kindName KCurve, "layer" .= planeName layer, "color" .= colorValue color, "size" .= sizeValue size, "opacity" .= opacityValue opacity, "points" .= list (map vecValue [a, c, b])]
        Fill FillItem {..} ->
            object
                [ "kind" .= kindName KFill
                , "layer" .= planeName layer
                , "color" .= colorValue color
                , "opacity" .= opacityValue opacity
                , "polygons" .= list [list (map vecValue (NonEmpty.toList p)) | p <- NonEmpty.toList polygons]
                ]
        TextBlock TextItem {..} ->
            object
                [ "kind" .= kindName KText
                , "layer" .= planeName layer
                , "text" .= text
                , "font" .= font
                , "style" .= textStyleName style
                , "size" .= number fontSize
                , "align" .= alignName align
                , "color" .= colorValue color
                , "opacity" .= opacityValue opacity
                , "center" .= vecValue center
                , "rotation" .= let Angle a = rotation in number a
                ]
        Picture ImageItem {..} ->
            object
                [ "kind" .= kindName KImage
                , "layer" .= planeName layer
                , "asset" .= let AssetId a = asset in a
                , "opacity" .= opacityValue opacity
                , "center" .= vecValue center
                , "width" .= let Extent e = width in number e
                , "height" .= let Extent e = height in number e
                , "rotation" .= let Angle a = rotation in number a
                , "flip" .= mirrored
                ]
        where
            sizeValue (BrushSize s) = number s
            opacityValue (Opacity o) = number o

-- | What only the whole item can say: a fill's total point budget.
checkFillBudget :: Item -> Either Text ()
checkFillBudget = \case
    Fill f ->
        when (sum (fmap length f.polygons) > maxPointsPerFill)
            $ Left ("expected at most " <> tshow maxPointsPerFill <> " points in a fill")
    _ -> Right ()

-- | A format 1 stroke: a pen line or an eraser path.
strokeV1From :: Value -> Either Text Item
strokeV1From =
    asObject >=> \o -> do
        isPen <- field "tool" toolV1 o
        c <- field "color" colorFrom o
        s <- field "size" brushSizeFrom o
        ps <- field "points" pointsFrom o
        pure
            $ if isPen
                then Stroke StrokeItem {layer = LinePlane, brush = Pen, color = c, size = s, opacity = Opacity 1, points = ps}
                else Erase EraseItem {size = s, points = ps}
    where
        toolV1 = \case
            String "pen" -> Right True
            String "eraser" -> Right False
            _ -> Left "expected a tool of \"pen\" or \"eraser\""

-- Rigs ----------------------------------------------------------------------------------

maxBones :: Int
maxBones = 64

-- | A bone at rest: its joint, length and direction; its parent comes before it.
data Bone = Bone {boneId :: LayerId, parent :: Maybe LayerId, origin :: Vec, boneLength :: Extent, boneAngle :: Angle}
    deriving stock (Eq, Show)

-- | A skeleton for a drawing layer, and which item of its rest drawing each bone carries.
data Rig = Rig [Bone] [(Int, LayerId)]
    deriving stock (Eq, Show)

-- | A drawing posed from the rig: the root moved, each bone turned.
data Pose = Pose Vec [(LayerId, Angle)]
    deriving stock (Eq, Show)

boneFrom :: Value -> Either Text Bone
boneFrom =
    asObject >=> \o ->
        Bone
            <$> field "id" layerIdFrom o
            <*> field "parent" (\case Null -> Right Nothing; v -> Just <$> layerIdFrom v) o
            <*> field "origin" vecFrom o
            <*> field "length" extentFrom o
            <*> field "angle" angleFrom o

bindingFrom :: Value -> Either Text (Int, LayerId)
bindingFrom =
    asObject >=> \o ->
        (,)
            <$> field "item" (asInteger >=> \n -> if n >= 0 && n < maxItemsPerFrame then Right n else Left ("expected an item from 0 to " <> tshow (maxItemsPerFrame - 1))) o
            <*> field "bone" layerIdFrom o

-- | Absent (or null) is no rig.
rigFrom :: Value -> Either Text (Maybe Rig)
rigFrom = \case
    Null -> Right Nothing
    v -> do
        o <- asObject v
        bones <- field "bones" (asArray >=> bounded "bones" 0 maxBones >=> indexed boneFrom) o
        bindings <- field "bindings" (asArray >=> bounded "bindings" 0 maxItemsPerFrame >=> indexed bindingFrom) o
        Just <$> checkedRig bones bindings

-- | Parents before children, ids distinct, bindings to bones that exist: the editor's rules and words.
checkedRig :: [Bone] -> [(Int, LayerId)] -> Either Text Rig
checkedRig bones bindings = go [] (zip [0 :: Int ..] bones)
    where
        go seen = \case
            [] -> do
                case [i | (i, (_, b)) <- zip [0 :: Int ..] bindings, b `notElem` seen] of
                    (i : _) -> Left ("bindings: " <> tshow i <> ": bone: expected a bone of this rig")
                    [] -> Right (Rig bones bindings)
            (i, b) : rest
                | b.boneId `elem` seen -> Left ("bones: " <> tshow i <> ": id: expected every bone id to be different")
                | Just pa <- b.parent, pa `notElem` seen -> Left ("bones: " <> tshow i <> ": parent: expected a bone defined before this one")
                | otherwise -> go (b.boneId : seen) rest

rigValue :: Rig -> Value
rigValue (Rig bones bindings) =
    object
        [ "bones" .= [object ["id" .= lid b.boneId, "parent" .= maybe Null (String . lid) b.parent, "origin" .= vecValue b.origin, "length" .= let Extent e = b.boneLength in number e, "angle" .= let Angle a = b.boneAngle in number a] | b <- bones]
        , "bindings" .= [object ["item" .= i, "bone" .= lid b] | (i, b) <- bindings]
        ]
    where
        lid (LayerId t) = t

-- | Absent (or null) is no pose.
poseFrom :: Value -> Either Text (Maybe Pose)
poseFrom = \case
    Null -> Right Nothing
    v -> do
        o <- asObject v
        root <- field "root" vecFrom o
        angles <- field "angles" (asArray >=> bounded "angles" 0 maxBones >=> indexed pair) o
        pure (Just (Pose root angles))
    where
        pair =
            asArray >=> \case
                [b, a] -> (,) <$> layerIdFrom b <*> angleFrom a
                _ -> Left "expected an angle as [bone, radians]"

poseValue :: Pose -> Value
poseValue (Pose root angles) = object ["root" .= vecValue root, "angles" .= [toJSONList' [String t, let Angle x = a in number x] | (LayerId t, a) <- angles]]
    where
        toJSONList' = Array . Vector.fromList

-- Cels ----------------------------------------------------------------------------------

-- | How many timeline frames one drawing is exposed for: 1 (on ones) to 24.
newtype Hold = Hold Int
    deriving stock (Eq, Ord, Show)

holdFrom :: Value -> Either Text Hold
holdFrom = asInteger >=> \n -> if n >= 1 && n <= 24 then Right (Hold n) else Left "expected a hold from 1 to 24 frames"

-- | The classic timing marks: keys, the breakdowns between them, and the inbetweens.
data Mark = NoMark | Key | Breakdown | Inbetween
    deriving stock (Bounded, Enum, Eq, Ord, Show)

markName :: Mark -> Text
markName = \case
    NoMark -> "none"
    Key -> "key"
    Breakdown -> "breakdown"
    Inbetween -> "inbetween"

-- | One drawing on a layer's timeline, held for its hold; posed ones carry their pose.
data Cel = Cel Hold Mark [Item] (Maybe Pose)
    deriving stock (Eq, Show)

itemsFrom :: Text -> (Value -> Either Text Item) -> Value -> Either Text [Item]
itemsFrom key itemFrom = asArray >=> bounded key 0 maxItemsPerFrame >=> indexed itemFrom

celV3From :: Value -> Either Text Cel
celV3From =
    asObject >=> \o ->
        Cel
            <$> field "hold" holdFrom o
            <*> field "mark" (oneOf markName "expected a mark of \"none\", \"key\", \"breakdown\" or \"inbetween\"") o
            <*> field "items" (itemsFrom "items" parse) o
            <*> field "pose" poseFrom o

-- | A format 2 drawing, or (with @strokes@) a format 1 one.
celOldFrom :: Text -> (Value -> Either Text Item) -> Value -> Either Text Cel
celOldFrom key itemFrom = asObject >=> \o -> Cel <$> field "hold" holdFrom o <*> pure NoMark <*> field key (itemsFrom key itemFrom) o <*> pure Nothing

celValue :: Cel -> Value
celValue (Cel (Hold n) m xs pose) = object (["hold" .= n, "mark" .= markName m, "items" .= map unparse xs] <> maybe [] (\p -> ["pose" .= poseValue p]) pose)

-- | A layer's cels; formats 1 and 2 called them frames, and their messages still do.
celsFrom :: Text -> (Value -> Either Text Cel) -> Value -> Either Text (NonEmpty Cel)
celsFrom what celFrom = asArray >=> bounded what 1 maxFrames >=> indexed celFrom >=> nonEmpty ("expected 1 to " <> tshow maxFrames <> " " <> what)

-- Layers --------------------------------------------------------------------------------

maxLayers :: Int
maxLayers = 20

slugFrom :: Text -> Int -> Value -> Either Text Text
slugFrom expected hi =
    asText >=> \s ->
        if Text.length s >= 1 && Text.length s <= hi && Text.all (\c -> isAsciiLower c || isDigit c || c == '-') s
            then Right s
            else Left expected

-- | A layer's or an audio track's id within its project.
newtype LayerId = LayerId Text
    deriving stock (Eq, Ord, Show)

layerIdFrom :: Value -> Either Text LayerId
layerIdFrom = fmap LayerId . slugFrom "expected a layer id of 1 to 32 of a-z, 0-9 and '-'" 32

-- | An uploaded file: a reference image or video, or audio.
newtype AssetId = AssetId Text
    deriving stock (Eq, Ord, Show)

instance Parse AssetId where
    type Raw AssetId = Text
    parse = fmap AssetId . slugFrom "expected an asset id of 1 to 64 of a-z, 0-9 and '-'" 64 . String
    unparse (AssetId a) = a

assetIdFrom :: Value -> Either Text AssetId
assetIdFrom = asText >=> parse

-- | A layer's or track's name: 1 to 40 characters (as the editor counts them), trimmed.
newtype Label = Label Text
    deriving stock (Eq, Ord, Show)

labelFrom :: Value -> Either Text Label
labelFrom =
    asText >=> \raw ->
        let
            t = Text.strip raw
        in
            if
                | Text.null t || utf16Length t > 40 -> Left "expected a name of 1 to 40 characters"
                | Text.any isControl t -> Left "expected a name without control characters"
                | otherwise -> Right (Label t)

boolFrom :: Value -> Either Text Bool
boolFrom = \case
    Bool b -> Right b
    _ -> Left "expected true or false"

data Media = Image | Video
    deriving stock (Bounded, Enum, Eq, Ord, Show)

mediaName :: Media -> Text
mediaName = Text.toLower . tshow

-- | What every layer has: an id, a name, and how it shows.
data LayerLook = LayerLook
    { layerId :: LayerId
    , label :: Label
    , visible :: Bool
    , locked :: Bool
    , opacity :: Opacity
    , blend :: Blend
    , clip :: Bool
    -- ^ Drawn only where the layers it clips to (the nearest unclipped one below) are.
    , motion :: Maybe Motion
    }
    deriving stock (Eq, Show)

plainLook :: LayerId -> Label -> LayerLook
plainLook i l = LayerLook i l True False (Opacity 1) Normal False Nothing

-- | How a layer mixes with the layers under it; on the wire only when not normal.
data Blend = Normal | Multiply | Screen | Overlay | Add | Darken | Lighten
    deriving stock (Bounded, Enum, Eq, Ord, Show)

blendName :: Blend -> Text
blendName = Text.toLower . tshow

blendFrom :: Value -> Either Text Blend
blendFrom = \case
    Null -> Right Normal
    v -> oneOf blendName "expected a blend of \"normal\", \"multiply\", \"screen\", \"overlay\", \"add\", \"darken\" or \"lighten\"" v

optionalBool :: Value -> Either Text Bool
optionalBool = \case
    Null -> Right False
    v -> boolFrom v

maxMotionKeys :: Int
maxMotionKeys = 1000

{- | A layer moved, scaled, turned and faded over time, about its pivot: each key holds
until it eases into the next.
-}
data Motion = Motion {pivot :: Vec, keys :: NonEmpty MotionKey}
    deriving stock (Eq, Show)

data MotionKey = MotionKey {frame :: Int, position :: Vec, scale :: Double, turn :: Angle, alpha :: Double, ease :: Ease}
    deriving stock (Eq, Show)

motionFrom :: Value -> Either Text (Maybe Motion)
motionFrom = \case
    Null -> Right Nothing
    v -> do
        o <- asObject v
        pv <- field "pivot" vecFrom o
        ks <- field "keys" (asArray >=> bounded "keys" 1 maxMotionKeys >=> indexed motionKeyFrom >=> nonEmpty ("expected 1 to " <> tshow maxMotionKeys <> " keys") >=> inOrder) o
        Right (Just (Motion pv ks))
    where
        inOrder ks
            | or (zipWith (\a b -> b.frame <= a.frame) (NonEmpty.toList ks) (NonEmpty.tail ks)) = Left "expected keys in frame order, one per frame"
            | otherwise = Right ks

motionKeyFrom :: Value -> Either Text MotionKey
motionKeyFrom =
    asObject >=> \o ->
        MotionKey
            <$> field "frame" frameIndexFrom o
            <*> field "position" vecFrom o
            <*> field "scale" (ranged Prelude.id 0.01 100 "expected a scale from 0.01 to 100") o
            <*> field "rotation" angleFrom o
            <*> field "opacity" (ranged Prelude.id 0 1 "expected an opacity from 0 to 1") o
            <*> field "ease" (oneOf easeName "expected an ease of \"linear\", \"in\", \"out\" or \"inOut\"") o

motionValue :: Motion -> Value
motionValue m =
    object
        [ "pivot" .= vecValue m.pivot
        , "keys" .= [let Angle a = k.turn in object ["frame" .= k.frame, "position" .= vecValue k.position, "scale" .= number k.scale, "rotation" .= number a, "opacity" .= number k.alpha, "ease" .= easeName k.ease] | k <- NonEmpty.toList m.keys]
        ]

data Layer
    = -- | Drawings on a timeline; holding the last one to the end makes a background.
      DrawingLayer LayerLook Bool (NonEmpty Cel) (Maybe Rig)
    | -- | An image or video to trace over; shown in the editor, never exported.
      ReferenceLayer LayerLook AssetId Media
    deriving stock (Eq, Show)

data LayerKind = KDrawing | KReference
    deriving stock (Bounded, Enum, Eq)

layerKindName :: LayerKind -> Text
layerKindName = \case
    KDrawing -> "drawing"
    KReference -> "reference"

layerFrom :: Value -> Either Text Layer
layerFrom =
    asObject >=> \o -> do
        kind <- field "kind" (oneOf layerKindName "expected a layer kind of \"drawing\" or \"reference\"") o
        look <-
            LayerLook
                <$> field "id" layerIdFrom o
                <*> field "name" labelFrom o
                <*> field "visible" boolFrom o
                <*> field "locked" boolFrom o
                <*> field "opacity" opacityFrom o
                <*> field "blend" blendFrom o
                <*> field "clip" optionalBool o
                <*> field "motion" motionFrom o
        case kind of
            KDrawing -> DrawingLayer look <$> field "holdLast" boolFrom o <*> field "cels" (celsFrom "cels" celV3From) o <*> field "rig" rigFrom o
            KReference -> ReferenceLayer look <$> field "asset" assetIdFrom o <*> field "media" (oneOf mediaName "expected media of \"image\" or \"video\"") o

layerValue :: Layer -> Value
layerValue l = object (look <> rest)
    where
        (kind, lk, rest) = case l of
            DrawingLayer x holdLast cels rig -> ("drawing" :: Text, x, ["holdLast" .= holdLast, "cels" .= map celValue (NonEmpty.toList cels)] <> maybe [] (\r -> ["rig" .= rigValue r]) rig)
            ReferenceLayer x (AssetId a) m -> ("reference", x, ["asset" .= a, "media" .= mediaName m])
        LayerId lid = lk.layerId
        Label nm = lk.label
        Opacity op = lk.opacity
        look =
            ["kind" .= kind, "id" .= lid, "name" .= nm, "visible" .= lk.visible, "locked" .= lk.locked, "opacity" .= number op]
                <> ["blend" .= blendName lk.blend | lk.blend /= Normal]
                <> ["clip" .= True | lk.clip]
                <> maybe [] (\m -> ["motion" .= motionValue m]) lk.motion

lookOf :: Layer -> LayerLook
lookOf = \case
    DrawingLayer x _ _ _ -> x
    ReferenceLayer x _ _ -> x

-- Camera --------------------------------------------------------------------------------

maxCameraKeys :: Int
maxCameraKeys = 1000

maxTimelineFrame :: Int
maxTimelineFrame = 99999

data Ease = Linear | EaseIn | EaseOut | EaseInOut
    deriving stock (Bounded, Enum, Eq, Ord, Show)

easeName :: Ease -> Text
easeName = \case
    Linear -> "linear"
    EaseIn -> "in"
    EaseOut -> "out"
    EaseInOut -> "inOut"

data CameraKey = CameraKey {frame :: Int, center :: Vec, zoom :: Double, turn :: Angle, ease :: Ease}
    deriving stock (Eq, Show)

frameIndexFrom :: Value -> Either Text Int
frameIndexFrom = asInteger >=> \n -> if n >= 0 && n <= maxTimelineFrame then Right n else Left ("expected a frame from 0 to " <> tshow maxTimelineFrame)

cameraKeyFrom :: Value -> Either Text CameraKey
cameraKeyFrom =
    asObject >=> \o ->
        CameraKey
            <$> field "frame" frameIndexFrom o
            <*> field "center" vecFrom o
            <*> field "zoom" (ranged Prelude.id 0.1 10 "expected a zoom from 0.1 to 10") o
            <*> field "rotation" angleFrom o
            <*> field "ease" (oneOf easeName "expected an ease of \"linear\", \"in\", \"out\" or \"inOut\"") o

cameraKeyValue :: CameraKey -> Value
cameraKeyValue k = let Angle a = k.turn in object ["frame" .= k.frame, "center" .= vecValue k.center, "zoom" .= number k.zoom, "rotation" .= number a, "ease" .= easeName k.ease]

-- Audio ---------------------------------------------------------------------------------

maxAudioTracks :: Int
maxAudioTracks = 8

data AudioTrack = AudioTrack {trackId :: LayerId, asset :: AssetId, trackName :: Label, offset :: Int, volume :: Double, muted :: Bool}
    deriving stock (Eq, Show)

audioTrackFrom :: Value -> Either Text AudioTrack
audioTrackFrom =
    asObject >=> \o ->
        AudioTrack
            <$> field "id" layerIdFrom o
            <*> field "asset" assetIdFrom o
            <*> field "name" labelFrom o
            <*> field "offset" (asInteger >=> \n -> if abs n <= maxTimelineFrame then Right n else Left ("expected an offset within " <> tshow maxTimelineFrame <> " frames")) o
            <*> field "volume" (ranged Prelude.id 0 1 "expected a volume from 0 to 1") o
            <*> field "muted" boolFrom o

audioTrackValue :: AudioTrack -> Value
audioTrackValue a =
    let
        LayerId tid = a.trackId
        AssetId aid = a.asset
        Label nm = a.trackName
    in
        object ["id" .= tid, "asset" .= aid, "name" .= nm, "offset" .= a.offset, "volume" .= number a.volume, "muted" .= a.muted]

-- Project -------------------------------------------------------------------------------

data Project = Project
    { id :: ProjectId
    , name :: ProjectName
    , canvas :: Canvas
    , fps :: Fps
    , layers :: NonEmpty Layer
    , camera :: [CameraKey]
    , audio :: [AudioTrack]
    , palette :: [Color]
    -- ^ The project's colours, to pick from and to recolour everything by.
    , fonts :: [ProjectFont]
    -- ^ Font files the project brings, by the family its text items name.
    }
    deriving stock (Eq, Show)
    deriving (FromJSON, ToJSON) via Parsed Project

data ProjectFont = ProjectFont {family :: Text, fontAsset :: AssetId}
    deriving stock (Eq, Show)

maxFonts :: Int
maxFonts = 32

fontsFrom :: Value -> Either Text [ProjectFont]
fontsFrom = \case
    Null -> Right []
    v -> (asArray >=> bounded "fonts" 0 maxFonts >=> indexed (asObject >=> \o -> ProjectFont <$> field "family" fontFamilyFrom o <*> field "asset" assetIdFrom o)) v

fontValue :: ProjectFont -> Value
fontValue f = let AssetId a = f.fontAsset in object ["family" .= f.family, "asset" .= a]

maxPalette :: Int
maxPalette = 64

paletteFrom :: Value -> Either Text [Color]
paletteFrom = \case
    Null -> Right []
    v -> (asArray >=> bounded "colours" 0 maxPalette >=> indexed colorFrom) v

projectId :: Project -> ProjectId
projectId p = p.id

-- | The rules that span fields, in the editor's order and words.
checked :: Project -> Either Text Project
checked p
    | length p.layers > maxLayers = Left ("layers: expected 1 to " <> tshow maxLayers <> " layers")
    | not (any isDrawingLayer p.layers) = Left "layers: expected at least one drawing layer"
    | distinct (map (layerIdText . lookOf) (NonEmpty.toList p.layers)) = Left "layers: expected every layer id to be different"
    | length p.camera > maxCameraKeys = Left ("camera: expected 0 to " <> tshow maxCameraKeys <> " keys")
    | or (zipWith (\a b -> b.frame <= a.frame) p.camera (drop 1 p.camera)) = Left "camera: expected keys in frame order, one per frame"
    | length p.audio > maxAudioTracks = Left ("audio: expected 0 to " <> tshow maxAudioTracks <> " tracks")
    | distinct (map (\a -> let LayerId t = a.trackId in t) p.audio) = Left "audio: expected every track id to be different"
    | distinct (map (.family) p.fonts) = Left "fonts: expected every font family to be different"
    | otherwise = Right p
    where
        distinct xs = length (nubOrd xs) /= length xs
        layerIdText l = let LayerId t = l.layerId in t

isDrawingLayer :: Layer -> Bool
isDrawingLayer = \case
    DrawingLayer {} -> True
    ReferenceLayer {} -> False

-- | Formats 1 and 2: one list of drawings, which becomes the first layer.
firstLayer :: NonEmpty Cel -> Layer
firstLayer cels = DrawingLayer (plainLook (LayerId "layer-1") (Label "Layer 1")) False cels Nothing

instance Parse Project where
    type Raw Project = Value
    parse =
        asObject >=> \o -> do
            version <- case KeyMap.lookup "format" o of
                Just (Number 1) -> Right (1 :: Int)
                Just (Number 2) -> Right 2
                Just (Number 3) -> Right 3
                _ -> Left "format: expected project format 1, 2 or 3"
            pid <- field "id" (asText >=> parse) o
            pname <- field "name" (asText >=> parse) o
            c <- field "canvas" canvasFrom o
            f <- field "fps" fpsFrom o
            if version < 3
                then do
                    cels <- field "frames" (celsFrom "frames" (if version == 1 then celOldFrom "strokes" strokeV1From else celOldFrom "items" parse)) o
                    checked Project {id = pid, name = pname, canvas = c, fps = f, layers = NonEmpty.singleton (firstLayer cels), camera = [], audio = [], palette = [], fonts = []}
                else do
                    ls <- field "layers" (asArray >=> bounded "layers" 1 maxLayers >=> indexed layerFrom >=> nonEmpty ("expected 1 to " <> tshow maxLayers <> " layers")) o
                    cam <- field "camera" (asArray >=> bounded "keys" 0 maxCameraKeys >=> indexed cameraKeyFrom) o
                    au <- field "audio" (asArray >=> bounded "tracks" 0 maxAudioTracks >=> indexed audioTrackFrom) o
                    pal <- field "palette" paletteFrom o
                    fs <- field "fonts" fontsFrom o
                    checked Project {id = pid, name = pname, canvas = c, fps = f, layers = ls, camera = cam, audio = au, palette = pal, fonts = fs}
    unparse p =
        object
            $ [ "format" .= (3 :: Int)
              , "id" .= unparse p.id
              , "name" .= unparse p.name
              , "canvas" .= canvasValue p.canvas
              , "fps" .= fpsValue p.fps
              , "layers" .= map layerValue (NonEmpty.toList p.layers)
              , "camera" .= map cameraKeyValue p.camera
              , "audio" .= map audioTrackValue p.audio
              ]
                <> ["palette" .= map colorValue p.palette | not (null p.palette)]
                <> ["fonts" .= map fontValue p.fonts | not (null p.fonts)]

-- | The timeline's length: its longest drawing layer.
duration :: Project -> Int
duration p = maximum (1 :| [sum [n | Cel (Hold n) _ _ _ <- NonEmpty.toList cels] | DrawingLayer _ _ cels _ <- NonEmpty.toList p.layers])

-- Edits (for agents) -------------------------------------------------------------------

-- | A new project with one drawing layer and one empty drawing.
newProject :: ProjectId -> ProjectName -> Canvas -> Fps -> Project
newProject pid pname c f = Project {id = pid, name = pname, canvas = c, fps = f, layers = NonEmpty.singleton (firstLayer (NonEmpty.singleton (Cel (Hold 1) NoMark [] Nothing))), camera = [], audio = [], palette = [], fonts = []}

{- | The layer an agent means: by id, or (none given) the topmost drawing layer. Returned
as its index, bottom first.
-}
pickLayer :: Maybe Text -> Project -> Either Text Int
pickLayer want p = case [i | (i, l) <- indexedLayers, isDrawingLayer l, maybe True (== layerIdText l) want] of
    [] -> Left (maybe "expected a drawing layer" ("expected a drawing layer with id " <>) want)
    is -> Right (last is)
    where
        indexedLayers = zip [0 ..] (NonEmpty.toList p.layers)
        layerIdText l = let LayerId t = (lookOf l).layerId in t

-- | The layers, for an agent: id, name, kind and number of drawings.
layersSummary :: Project -> Value
layersSummary p =
    toJSONList
        [ object (["index" .= i, "id" .= t, "name" .= nm, "kind" .= kind] <> extra)
        | (i, l) <- zip [0 :: Int ..] (NonEmpty.toList p.layers)
        , let
            LayerId t = (lookOf l).layerId
            Label nm = (lookOf l).label
            (kind, extra) = case l of
                DrawingLayer _ holdLast cels rig -> ("drawing" :: Text, ["drawings" .= length cels, "holdLast" .= holdLast, "rigged" .= isJust rig])
                ReferenceLayer {} -> ("reference", [])
        ]
    where
        toJSONList = Array . Vector.fromList

onLayerCels :: Int -> (NonEmpty Cel -> Either Text [Cel]) -> Project -> Either Text Project
onLayerCels li f p = do
    ls <- traverse step (zip [0 ..] (NonEmpty.toList p.layers))
    layers' <- nonEmpty "expected a layer" ls
    checked p {layers = layers'}
    where
        step (i, l) = case l of
            DrawingLayer look holdLast cels rig | i == li -> do
                cs <- f cels
                when (length cs > maxFrames) $ Left ("expected at most " <> tshow maxFrames <> " drawings on a layer")
                ne <- nonEmpty "expected to keep at least one drawing on a layer" cs
                pure (DrawingLayer look holdLast ne rig)
            _ -> Right l

celsOf :: Int -> Project -> [Cel]
celsOf li p = case drop li (NonEmpty.toList p.layers) of
    (DrawingLayer _ _ cels _ : _) -> NonEmpty.toList cels
    _ -> []

-- | Drawing @n@ (from 1) of a layer, as wire JSON.
drawingAt :: Int -> Int -> Project -> Either Text Value
drawingAt li n p = case drop (n - 1) (celsOf li p) of
    (c : _) | n >= 1 -> Right (celValue c)
    _ -> Left ("expected a drawing from 1 to " <> tshow (length (celsOf li p)))

onDrawing :: Int -> Int -> ([Item] -> [Item]) -> Project -> Either Text Project
onDrawing li n f p
    | n < 1 || n > length (celsOf li p) = Left ("expected a drawing from 1 to " <> tshow (length (celsOf li p)))
    | otherwise = onLayerCels li (traverse step . zip [1 ..] . NonEmpty.toList) p
    where
        -- An agent's edit to a posed drawing makes it an ordinary drawing again.
        step (i, Cel h m xs pose)
            | i == n = let ys = f xs in if length ys > maxItemsPerFrame then Left ("expected 0 to " <> tshow maxItemsPerFrame <> " items") else Right (Cel h m ys Nothing)
            | otherwise = Right (Cel h m xs pose)

-- | Items on top of drawing @n@ of a layer.
addItemsAt :: Int -> Int -> [Item] -> Project -> Either Text Project
addItemsAt li n new = onDrawing li n (<> new)

-- | Drawing @n@'s items, all replaced.
replaceItemsAt :: Int -> Int -> [Item] -> Project -> Either Text Project
replaceItemsAt li n new = onDrawing li n (const new)

-- | A drawing after drawing @after@ of a layer (0 puts it first): blank, or a copy of drawing @after@.
insertDrawing :: Int -> Int -> Bool -> Int -> Project -> Either Text Project
insertDrawing li after copy holdFor p
    | after < 0 || after > length (celsOf li p) = Left ("expected a position from 0 to " <> tshow (length (celsOf li p)))
    | otherwise = do
        h <- holdFrom (Number (fromIntegral holdFor))
        onLayerCels
            li
            ( \cels ->
                let
                    cs = NonEmpty.toList cels
                    source = case drop (after - 1) cs of
                        (Cel _ m xs pose : _) | copy && after >= 1 -> Cel h m xs pose
                        _ -> Cel h NoMark [] Nothing
                    (before, rest) = splitAt after cs
                in
                    Right (before <> [source] <> rest)
            )
            p

-- | A new drawing layer on top; with @background@, at the bottom and held to the end.
addLayer :: Text -> Bool -> Project -> Either Text Project
addLayer raw background p = do
    lbl <- labelFrom (String raw)
    let
        ids =
            [ t
            | l <- NonEmpty.toList p.layers
            , let
                LayerId t = (lookOf l).layerId
            ]
        fresh =
            head'
                [ c
                | n <- [length ids + 1 ..]
                , let
                    c = "layer-" <> tshow (n :: Int)
                , c `notElem` ids
                ]
        layer = DrawingLayer (plainLook (LayerId fresh) lbl) background (NonEmpty.singleton (Cel (Hold 1) NoMark [] Nothing)) Nothing
        ls = NonEmpty.toList p.layers
        (refs, others) = break isDrawingLayer ls
    layers' <- nonEmpty "expected a layer" (if background then refs <> [layer] <> others else ls <> [layer])
    checked p {layers = layers'}
    where
        head' = \case
            (x : _) -> x
            [] -> "layer-new"

-- Revisions -----------------------------------------------------------------------------

{- | How many times a project has been saved. A save names the revision it started from,
and the store refuses it if another save got there first, so the editor on two devices
and an agent over MCP never silently overwrite each other.
-}
newtype Revision = Revision Int
    deriving stock (Eq, Ord, Show)
    deriving (FromJSON, ToJSON) via Parsed Revision
    deriving (FromHttpApiData, ToHttpApiData) via Parsed Revision

instance Parse Revision where
    type Raw Revision = Int
    parse n = if n >= 0 then Right (Revision n) else Left "expected a revision of 0 or more"
    unparse (Revision n) = n

firstRevision :: Revision
firstRevision = Revision 1

nextRevision :: Revision -> Revision
nextRevision (Revision n) = Revision (n + 1)

-- Summaries -----------------------------------------------------------------------------

{- | What the project list shows, kept beside each project so listing never reads the
drawings themselves.
-}
data ProjectSummary = ProjectSummary
    { id :: ProjectId
    , name :: ProjectName
    , canvas :: Canvas
    , fps :: Fps
    , frameCount :: Int
    , revision :: Revision
    }
    deriving stock (Eq, Show)
    deriving (FromJSON, ToJSON) via Parsed ProjectSummary

summarize :: Revision -> Project -> ProjectSummary
summarize r p = ProjectSummary {id = p.id, name = p.name, canvas = p.canvas, fps = p.fps, frameCount = duration p, revision = r}

summaryId :: ProjectSummary -> ProjectId
summaryId s = s.id

summaryName :: ProjectSummary -> ProjectName
summaryName s = s.name

summaryRevision :: ProjectSummary -> Revision
summaryRevision s = s.revision

data RawSummary = RawSummary
    { id :: Text
    , name :: Text
    , canvas :: RawCanvas
    , fps :: Int
    , frameCount :: Int
    , revision :: Maybe Int
    -- ^ Missing in summaries written before revisions existed: those read as 0.
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance Parse ProjectSummary where
    type Raw ProjectSummary = RawSummary
    parse raw = do
        pid <- at "id" (parse raw.id)
        pname <- at "name" (parse raw.name)
        c <- at "canvas" (canvasFrom (object ["width" .= raw.canvas.width, "height" .= raw.canvas.height]))
        f <- at "fps" (fpsFrom (Number (fromIntegral raw.fps)))
        unless (raw.frameCount >= 1 && raw.frameCount <= maxFrames)
            $ Left ("frameCount: expected a frame count from 1 to " <> tshow maxFrames)
        r <- at "revision" (parse (fromMaybe 0 raw.revision))
        pure ProjectSummary {id = pid, name = pname, canvas = c, fps = f, frameCount = raw.frameCount, revision = r}
    unparse s =
        let
            Canvas (Width w) (Height h) = s.canvas
        in
            RawSummary
                { id = unparse s.id
                , name = unparse s.name
                , canvas = RawCanvas {width = w, height = h}
                , fps = fpsValue s.fps
                , frameCount = s.frameCount
                , revision = Just (unparse s.revision)
                }
