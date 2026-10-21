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
    , Frame

      -- * Project
    , Project
    , projectId
    , newProject
    , drawingCount
    , drawingAt
    , addItemsAt
    , replaceItemsAt
    , insertDrawing
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
import Data.Char (isAsciiLower, isControl, isDigit, isHexDigit, ord, toLower)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
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

-- | Colour renders under line, always.
data Layer = LineLayer | ColorLayer
    deriving stock (Bounded, Enum, Eq, Ord, Show)

layerName :: Layer -> Text
layerName = \case
    LineLayer -> "line"
    ColorLayer -> "color"

layerFrom :: Value -> Either Text Layer
layerFrom = oneOf layerName "expected a layer of \"line\" or \"color\""

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
data StrokeItem = StrokeItem {layer :: Layer, brush :: Brush, color :: Color, size :: BrushSize, opacity :: Opacity, points :: NonEmpty Point}
    deriving stock (Eq, Show)

-- | The pixel eraser's path; it cuts both layers at its place in the order.
data EraseItem = EraseItem {size :: BrushSize, points :: NonEmpty Point}
    deriving stock (Eq, Show)

-- | A path stamped with grass, birds, stars and the like.
data ScatterItem = ScatterItem {layer :: Layer, brush :: Scatter, color :: Color, size :: BrushSize, opacity :: Opacity, seed :: Seed, points :: NonEmpty Point}
    deriving stock (Eq, Show)

-- | A line, rectangle, ellipse or triangle, by centre, size and turn.
data ShapeItem = ShapeItem
    { layer :: Layer
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
data CurveItem = CurveItem {layer :: Layer, color :: Color, size :: BrushSize, opacity :: Opacity, points :: (Vec, Vec, Vec)}
    deriving stock (Eq, Show)

-- | A paint-bucket fill: outlines filled even-odd, so holes stay open.
data FillItem = FillItem {layer :: Layer, color :: Color, opacity :: Opacity, polygons :: NonEmpty (NonEmpty Vec)}
    deriving stock (Eq, Show)

data Item
    = Stroke StrokeItem
    | Erase EraseItem
    | ScatterStroke ScatterItem
    | Shape ShapeItem
    | Curve CurveItem
    | Fill FillItem
    deriving stock (Eq, Show)
    deriving (FromJSON, ToJSON) via Parsed Item

data Kind = KStroke | KErase | KScatter | KShape | KCurve | KFill
    deriving stock (Bounded, Enum, Eq)

kindName :: Kind -> Text
kindName = \case
    KStroke -> "stroke"
    KErase -> "erase"
    KScatter -> "scatter"
    KShape -> "shape"
    KCurve -> "curve"
    KFill -> "fill"

instance Parse Item where
    type Raw Item = Value
    parse v = do
        o <- asObject v
        kind <- field "kind" (oneOf kindName "expected an item kind of \"stroke\", \"erase\", \"scatter\", \"shape\", \"curve\" or \"fill\"") o
        item <- case kind of
            KStroke ->
                fmap Stroke
                    $ StrokeItem
                        <$> field "layer" layerFrom o
                        <*> field "brush" (oneOf brushName "expected a brush of \"pen\", \"pencil\", \"marker\" or \"paint\"") o
                        <*> field "color" colorFrom o
                        <*> field "size" brushSizeFrom o
                        <*> field "opacity" opacityFrom o
                        <*> field "points" pointsFrom o
            KErase -> fmap Erase $ EraseItem <$> field "size" brushSizeFrom o <*> field "points" pointsFrom o
            KScatter ->
                fmap ScatterStroke
                    $ ScatterItem
                        <$> field "layer" layerFrom o
                        <*> field "brush" (oneOf scatterName "expected a scatter brush of \"grass\", \"birds\", \"stars\", \"leaves\", \"flowers\" or \"spray\"") o
                        <*> field "color" colorFrom o
                        <*> field "size" brushSizeFrom o
                        <*> field "opacity" opacityFrom o
                        <*> field "seed" seedFrom o
                        <*> field "points" pointsFrom o
            KShape ->
                fmap Shape
                    $ ShapeItem
                        <$> field "layer" layerFrom o
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
                        <$> field "layer" layerFrom o
                        <*> field "color" colorFrom o
                        <*> field "size" brushSizeFrom o
                        <*> field "opacity" opacityFrom o
                        <*> field "points" curveFrom o
            KFill ->
                fmap Fill
                    $ FillItem <$> field "layer" layerFrom o <*> field "color" colorFrom o <*> field "opacity" opacityFrom o <*> field "polygons" polygonsFrom o
        checkFillBudget item
        pure item
    unparse = \case
        Stroke StrokeItem {..} ->
            object ["kind" .= kindName KStroke, "layer" .= layerName layer, "brush" .= brushName brush, "color" .= colorValue color, "size" .= sizeValue size, "opacity" .= opacityValue opacity, "points" .= pointsValue points]
        Erase EraseItem {..} -> object ["kind" .= kindName KErase, "size" .= sizeValue size, "points" .= pointsValue points]
        ScatterStroke ScatterItem {..} ->
            object
                [ "kind" .= kindName KScatter
                , "layer" .= layerName layer
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
                , "layer" .= layerName layer
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
                object ["kind" .= kindName KCurve, "layer" .= layerName layer, "color" .= colorValue color, "size" .= sizeValue size, "opacity" .= opacityValue opacity, "points" .= list (map vecValue [a, c, b])]
        Fill FillItem {..} ->
            object
                [ "kind" .= kindName KFill
                , "layer" .= layerName layer
                , "color" .= colorValue color
                , "opacity" .= opacityValue opacity
                , "polygons" .= list [list (map vecValue (NonEmpty.toList p)) | p <- NonEmpty.toList polygons]
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
                then Stroke StrokeItem {layer = LineLayer, brush = Pen, color = c, size = s, opacity = Opacity 1, points = ps}
                else Erase EraseItem {size = s, points = ps}
    where
        toolV1 = \case
            String "pen" -> Right True
            String "eraser" -> Right False
            _ -> Left "expected a tool of \"pen\" or \"eraser\""

-- Frames --------------------------------------------------------------------------------

-- | How many timeline frames one drawing is exposed for: 1 (on ones) to 24.
newtype Hold = Hold Int
    deriving stock (Eq, Ord, Show)

holdFrom :: Value -> Either Text Hold
holdFrom = asInteger >=> \n -> if n >= 1 && n <= 24 then Right (Hold n) else Left "expected a hold from 1 to 24 frames"

data Frame = Frame Hold [Item]
    deriving stock (Eq, Show)

-- | A drawing, its items read from the list named @key@ (@items@, or format 1's @strokes@).
frameFrom :: Text -> (Value -> Either Text Item) -> Value -> Either Text Frame
frameFrom key itemFrom =
    asObject >=> \o -> Frame <$> field "hold" holdFrom o <*> field key (asArray >=> bounded key 0 maxItemsPerFrame >=> indexed itemFrom) o

frameValue :: Frame -> Value
frameValue (Frame (Hold n) xs) = object ["hold" .= n, "items" .= map unparse xs]

-- Project -------------------------------------------------------------------------------

data Project = Project
    { id :: ProjectId
    , name :: ProjectName
    , canvas :: Canvas
    , fps :: Fps
    , frames :: NonEmpty Frame
    }
    deriving stock (Eq, Show)
    deriving (FromJSON, ToJSON) via Parsed Project

projectId :: Project -> ProjectId
projectId p = p.id

instance Parse Project where
    type Raw Project = Value
    parse =
        asObject >=> \o -> do
            frame <- case KeyMap.lookup "format" o of
                Just (Number 1) -> Right (frameFrom "strokes" strokeV1From)
                Just (Number 2) -> Right (frameFrom "items" parse)
                _ -> Left "format: expected project format 1 or 2"
            Project
                <$> field "id" (asText >=> parse) o
                <*> field "name" (asText >=> parse) o
                <*> field "canvas" canvasFrom o
                <*> field "fps" fpsFrom o
                <*> field "frames" (asArray >=> bounded "frames" 1 maxFrames >=> indexed frame >=> nonEmpty "expected a frame") o
    unparse p =
        object
            [ "format" .= (2 :: Int)
            , "id" .= unparse p.id
            , "name" .= unparse p.name
            , "canvas" .= canvasValue p.canvas
            , "fps" .= fpsValue p.fps
            , "frames" .= map frameValue (NonEmpty.toList p.frames)
            ]

-- Edits (for agents) -------------------------------------------------------------------

-- | A new project with one empty drawing.
newProject :: ProjectId -> ProjectName -> Canvas -> Fps -> Project
newProject pid pname c f = Project {id = pid, name = pname, canvas = c, fps = f, frames = NonEmpty.singleton (Frame (Hold 1) [])}

drawingCount :: Project -> Int
drawingCount p = length p.frames

-- | The hold and items of drawing @n@ (from 1), as wire JSON.
drawingAt :: Int -> Project -> Either Text Value
drawingAt n p = case drop (n - 1) (NonEmpty.toList p.frames) of
    (f : _) | n >= 1 -> Right (frameValue f)
    _ -> Left ("expected a drawing from 1 to " <> tshow (length p.frames))

onDrawing :: Int -> ([Item] -> Either Text [Item]) -> Project -> Either Text Project
onDrawing n f p
    | n < 1 || n > length p.frames = Left ("expected a drawing from 1 to " <> tshow (length p.frames))
    | otherwise = do
        fs <- traverse (\(i, Frame h xs) -> if i == n then Frame h <$> (f xs >>= limit) else Right (Frame h xs)) (zip [1 ..] (NonEmpty.toList p.frames))
        frames' <- nonEmpty "expected a frame" fs
        pure p {frames = frames'}
    where
        limit xs = if length xs > maxItemsPerFrame then Left ("expected 0 to " <> tshow maxItemsPerFrame <> " items") else Right xs

-- | Items on top of drawing @n@.
addItemsAt :: Int -> [Item] -> Project -> Either Text Project
addItemsAt n new = onDrawing n (Right . (<> new))

-- | Drawing @n@'s items, all replaced.
replaceItemsAt :: Int -> [Item] -> Project -> Either Text Project
replaceItemsAt n new = onDrawing n (const (Right new))

-- | A drawing after drawing @n@ (0 puts it first): blank, or a copy of drawing @n@.
insertDrawing :: Int -> Bool -> Int -> Project -> Either Text Project
insertDrawing after copy holdFor p
    | after < 0 || after > length p.frames = Left ("expected a position from 0 to " <> tshow (length p.frames))
    | length p.frames >= maxFrames = Left ("expected at most " <> tshow maxFrames <> " frames")
    | otherwise = do
        h <- holdFrom (Number (fromIntegral holdFor))
        let
            fs = NonEmpty.toList p.frames
            source = if copy && after >= 1 then maybe [] (\(Frame _ xs) -> xs) (lookupAt (after - 1) fs) else []
            (before, rest) = splitAt after fs
        frames' <- nonEmpty "expected a frame" (before <> [Frame h source] <> rest)
        pure p {frames = frames'}
    where
        lookupAt i xs = case drop i xs of
            (x : _) -> Just x
            [] -> Nothing

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
summarize r p = ProjectSummary {id = p.id, name = p.name, canvas = p.canvas, fps = p.fps, frameCount = length p.frames, revision = r}

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
