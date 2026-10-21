{-# LANGUAGE TypeFamilies #-}

{- | An animation project: a canvas, a frame rate and the frames drawn on it.

Every type here is built by 'parse' and exported abstractly. The wire shape is the @Raw…@
record next to each one; the browser editor (@web/src/domain@) parses the same shape into
its own branded types, so the two sides agree on what a valid project is.
-}
module Anima.Types.Project
    ( -- * Identity
      ProjectId
    , ProjectName

      -- * Settings
    , Fps
    , fpsValue
    , Width
    , Height
    , Canvas
    , canvasWidth
    , canvasHeight

      -- * Drawing
    , Tool (..)
    , Color
    , BrushSize
    , Pressure
    , Point
    , Stroke
    , strokeTool
    , strokePoints
    , Hold
    , Frame
    , frameHold
    , frameStrokes

      -- * Project
    , Format (..)
    , Project
    , projectId
    , projectName
    , projectCanvas
    , projectFps
    , projectFrames
    , ProjectSummary
    , summarize
    , summaryId
    , summaryName

      -- * Wire shapes
    , RawCanvas (..)
    , RawStroke (..)
    , RawFrame (..)
    , RawProject (..)
    , RawSummary (..)

      -- * Limits
    , maxFrames
    , maxStrokesPerFrame
    , maxPointsPerStroke
    )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAsciiLower, isControl, isDigit, isHexDigit, toLower)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Traversable (mapAccumL)
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text

import Anima.Types.Parse (Parse (..), Parsed (..))

-- | The most frames one project may have: about seven minutes at 24 fps.
maxFrames :: Int
maxFrames = 10000

maxStrokesPerFrame :: Int
maxStrokesPerFrame = 5000

maxPointsPerStroke :: Int
maxPointsPerStroke = 20000

-- Identity ------------------------------------------------------------------------------

{- | Chosen by the editor when a project is created (a UUID, in practice): 1 to 64 of
@a–z@, @0–9@ and @-@, so it is also a safe file name.
-}
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

-- | What the project is called: 1 to 80 characters, no control characters, trimmed.
newtype ProjectName = ProjectName Text
    deriving stock (Eq, Ord, Show)
    deriving (FromJSON, ToJSON) via Parsed ProjectName

instance Parse ProjectName where
    type Raw ProjectName = Text
    parse raw
        | Text.null trimmed || Text.length trimmed > 80 = Left "expected a project name of 1 to 80 characters"
        | Text.any isControl trimmed = Left "expected a project name without control characters"
        | otherwise = Right (ProjectName trimmed)
        where
            trimmed = Text.strip raw
    unparse (ProjectName raw) = raw

-- Settings ------------------------------------------------------------------------------

-- | The frame rates the editor and the renderer support.
data Fps = Fps12 | Fps24 | Fps25 | Fps30 | Fps48 | Fps50 | Fps60
    deriving stock (Bounded, Enum, Eq, Ord, Show)
    deriving (FromJSON, ToJSON) via Parsed Fps

fpsValue :: Fps -> Int
fpsValue = \case
    Fps12 -> 12
    Fps24 -> 24
    Fps25 -> 25
    Fps30 -> 30
    Fps48 -> 48
    Fps50 -> 50
    Fps60 -> 60

instance Parse Fps where
    type Raw Fps = Int
    parse raw = case filter ((== raw) . fpsValue) [minBound .. maxBound] of
        [fps] -> Right fps
        _ -> Left "expected a frame rate of 12, 24, 25, 30, 48, 50 or 60"
    unparse = fpsValue

{- | Canvas width in pixels: even (video encoders refuse odd sizes) and from 64 to 7680.

Width and height are separate types so the two can never be swapped.
-}
newtype Width = Width Int
    deriving stock (Eq, Ord, Show)
    deriving (FromJSON, ToJSON) via Parsed Width

instance Parse Width where
    type Raw Width = Int
    parse = fmap Width . dimension "width"
    unparse (Width raw) = raw

-- | Canvas height in pixels, on the same terms as 'Width'.
newtype Height = Height Int
    deriving stock (Eq, Ord, Show)
    deriving (FromJSON, ToJSON) via Parsed Height

instance Parse Height where
    type Raw Height = Int
    parse = fmap Height . dimension "height"
    unparse (Height raw) = raw

dimension :: Text -> Int -> Either Text Int
dimension what raw
    | raw < 64 || raw > 7680 = Left ("expected a " <> what <> " from 64 to 7680 pixels")
    | odd raw = Left ("expected an even " <> what)
    | otherwise = Right raw

data Canvas = Canvas {width :: Width, height :: Height}
    deriving stock (Eq, Show)
    deriving (FromJSON, ToJSON) via Parsed Canvas

canvasWidth :: Canvas -> Width
canvasWidth c = c.width

canvasHeight :: Canvas -> Height
canvasHeight c = c.height

data RawCanvas = RawCanvas {width :: Int, height :: Int}
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance Parse Canvas where
    type Raw Canvas = RawCanvas
    parse raw = Canvas <$> at "width" (parse raw.width) <*> at "height" (parse raw.height)
    unparse c = RawCanvas {width = unparse c.width, height = unparse c.height}

-- Drawing -------------------------------------------------------------------------------

-- | What a stroke does: lay down ink, or erase what is under it.
data Tool = Pen | Eraser
    deriving stock (Bounded, Enum, Eq, Ord, Show)
    deriving (FromJSON, ToJSON) via Parsed Tool

instance Parse Tool where
    type Raw Tool = Text
    parse = \case
        "pen" -> Right Pen
        "eraser" -> Right Eraser
        _ -> Left "expected a tool of \"pen\" or \"eraser\""
    unparse = \case
        Pen -> "pen"
        Eraser -> "eraser"

-- | An sRGB colour, @#rrggbb@; kept lower-case.
newtype Color = Color Text
    deriving stock (Eq, Ord, Show)
    deriving (FromJSON, ToJSON) via Parsed Color

instance Parse Color where
    type Raw Color = Text
    parse raw = case Text.uncons raw of
        Just ('#', hex)
            | Text.length hex == 6 && Text.all isHexDigit hex -> Right (Color (Text.map toLower raw))
        _ -> Left "expected a colour as #rrggbb"
    unparse (Color raw) = raw

-- | Brush diameter in canvas pixels at full pressure, from 0.5 to 200.
newtype BrushSize = BrushSize Double
    deriving stock (Eq, Ord, Show)
    deriving (FromJSON, ToJSON) via Parsed BrushSize

instance Parse BrushSize where
    type Raw BrushSize = Double
    parse raw
        | finite raw && raw >= 0.5 && raw <= 200 = Right (BrushSize raw)
        | otherwise = Left "expected a brush size from 0.5 to 200"
    unparse (BrushSize raw) = raw

-- | Pen pressure, from 0 to 1.
newtype Pressure = Pressure Double
    deriving stock (Eq, Ord, Show)
    deriving (FromJSON, ToJSON) via Parsed Pressure

instance Parse Pressure where
    type Raw Pressure = Double
    parse raw
        | finite raw && raw >= 0 && raw <= 1 = Right (Pressure raw)
        | otherwise = Left "expected a pressure from 0 to 1"
    unparse (Pressure raw) = raw

{- | One pen sample in canvas pixels. Coordinates may lie off the canvas (a stroke can run
past its edge), but no further than 100 000 pixels from the origin.
-}
data Point = Point Double Double Pressure
    deriving stock (Eq, Show)

instance Parse Point where
    type Raw Point = (Double, Double, Double)
    parse (x, y, p)
        | not (coordinate x && coordinate y) = Left "expected coordinates within 100000 pixels of the origin"
        | otherwise = Point x y <$> parse p
        where
            coordinate v = finite v && abs v <= 100000
    unparse (Point x y p) = (x, y, unparse p)

data Stroke = Stroke
    { tool :: Tool
    , color :: Color
    , size :: BrushSize
    , points :: NonEmpty Point
    }
    deriving stock (Eq, Show)
    deriving (FromJSON, ToJSON) via Parsed Stroke

strokeTool :: Stroke -> Tool
strokeTool s = s.tool

strokePoints :: Stroke -> NonEmpty Point
strokePoints s = s.points

data RawStroke = RawStroke
    { tool :: Text
    , color :: Text
    , size :: Double
    , points :: [(Double, Double, Double)]
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance Parse Stroke where
    type Raw Stroke = RawStroke
    parse raw =
        Stroke
            <$> at "tool" (parse raw.tool)
            <*> at "color" (parse raw.color)
            <*> at "size" (parse raw.size)
            <*> at "points" (boundedNonEmpty "points" maxPointsPerStroke raw.points >>= indexed parse)
    unparse s =
        RawStroke
            { tool = unparse s.tool
            , color = unparse s.color
            , size = unparse s.size
            , points = map unparse (NonEmpty.toList s.points)
            }

-- | How many frames of the timeline one drawing is exposed for: 1 (on ones) to 24.
newtype Hold = Hold Int
    deriving stock (Eq, Ord, Show)
    deriving (FromJSON, ToJSON) via Parsed Hold

instance Parse Hold where
    type Raw Hold = Int
    parse raw
        | raw >= 1 && raw <= 24 = Right (Hold raw)
        | otherwise = Left "expected a hold from 1 to 24 frames"
    unparse (Hold raw) = raw

data Frame = Frame {hold :: Hold, strokes :: [Stroke]}
    deriving stock (Eq, Show)
    deriving (FromJSON, ToJSON) via Parsed Frame

frameHold :: Frame -> Hold
frameHold f = f.hold

frameStrokes :: Frame -> [Stroke]
frameStrokes f = f.strokes

data RawFrame = RawFrame {hold :: Int, strokes :: [RawStroke]}
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance Parse Frame where
    type Raw Frame = RawFrame
    parse raw =
        Frame
            <$> at "hold" (parse raw.hold)
            <*> at "strokes" (bounded "strokes" 0 maxStrokesPerFrame raw.strokes >>= indexed parse)
    unparse f = RawFrame {hold = unparse f.hold, strokes = map unparse f.strokes}

-- Project -------------------------------------------------------------------------------

-- | The on-disk and on-the-wire format. A new format is a new constructor, never a reinterpretation.
data Format = Format1
    deriving stock (Bounded, Enum, Eq, Ord, Show)

instance Parse Format where
    type Raw Format = Int
    parse = \case
        1 -> Right Format1
        _ -> Left "expected project format 1"
    unparse Format1 = 1

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

projectName :: Project -> ProjectName
projectName p = p.name

projectCanvas :: Project -> Canvas
projectCanvas p = p.canvas

projectFps :: Project -> Fps
projectFps p = p.fps

projectFrames :: Project -> NonEmpty Frame
projectFrames p = p.frames

data RawProject = RawProject
    { format :: Int
    , id :: Text
    , name :: Text
    , canvas :: RawCanvas
    , fps :: Int
    , frames :: [RawFrame]
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance Parse Project where
    type Raw Project = RawProject
    parse raw = do
        Format1 <- at "format" (parse raw.format)
        Project
            <$> at "id" (parse raw.id)
            <*> at "name" (parse raw.name)
            <*> at "canvas" (parse raw.canvas)
            <*> at "fps" (parse raw.fps)
            <*> at "frames" (boundedNonEmpty "frames" maxFrames raw.frames >>= indexed parse)
    unparse p =
        RawProject
            { format = unparse Format1
            , id = unparse p.id
            , name = unparse p.name
            , canvas = unparse p.canvas
            , fps = unparse p.fps
            , frames = map unparse (NonEmpty.toList p.frames)
            }

{- | What the project list shows, kept beside each project so listing never reads the
drawings themselves.
-}
data ProjectSummary = ProjectSummary
    { id :: ProjectId
    , name :: ProjectName
    , canvas :: Canvas
    , fps :: Fps
    , frameCount :: Int
    }
    deriving stock (Eq, Show)
    deriving (FromJSON, ToJSON) via Parsed ProjectSummary

summarize :: Project -> ProjectSummary
summarize p =
    ProjectSummary
        { id = p.id
        , name = p.name
        , canvas = p.canvas
        , fps = p.fps
        , frameCount = length p.frames
        }

summaryId :: ProjectSummary -> ProjectId
summaryId s = s.id

summaryName :: ProjectSummary -> ProjectName
summaryName s = s.name

data RawSummary = RawSummary
    { id :: Text
    , name :: Text
    , canvas :: RawCanvas
    , fps :: Int
    , frameCount :: Int
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance Parse ProjectSummary where
    type Raw ProjectSummary = RawSummary
    parse raw =
        ProjectSummary
            <$> at "id" (parse raw.id)
            <*> at "name" (parse raw.name)
            <*> at "canvas" (parse raw.canvas)
            <*> at "fps" (parse raw.fps)
            <*> at "frameCount" frameCount
        where
            frameCount
                | raw.frameCount >= 1 && raw.frameCount <= maxFrames = Right raw.frameCount
                | otherwise = Left ("expected a frame count from 1 to " <> Text.pack (show maxFrames))
    unparse s =
        RawSummary
            { id = unparse s.id
            , name = unparse s.name
            , canvas = unparse s.canvas
            , fps = unparse s.fps
            , frameCount = s.frameCount
            }

-- Helpers -------------------------------------------------------------------------------

-- | Prefix a rejection with where it happened; the path is ours, never the input.
at :: Text -> Either Text a -> Either Text a
at field = either (Left . ((field <> ": ") <>)) Right

-- | Parse each element, naming the index of the first that fails.
indexed :: (Traversable t) => (r -> Either Text a) -> t r -> Either Text (t a)
indexed f = traverse (\(i, x) -> at (Text.pack (show i)) (f x)) . snd . mapAccumL (\i x -> (i + 1, (i :: Int, x))) 0

bounded :: Text -> Int -> Int -> [a] -> Either Text [a]
bounded what lo hi xs
    | n < lo || n > hi = Left ("expected " <> Text.pack (show lo) <> " to " <> Text.pack (show hi) <> " " <> what)
    | otherwise = Right xs
    where
        n = length xs

-- | 'bounded' with a lower bound of one, as the evidence it is.
boundedNonEmpty :: Text -> Int -> [a] -> Either Text (NonEmpty a)
boundedNonEmpty what hi xs = bounded what 1 hi xs >>= maybe (Left ("expected at least one of " <> what)) Right . NonEmpty.nonEmpty

finite :: Double -> Bool
finite v = not (isNaN v || isInfinite v)
