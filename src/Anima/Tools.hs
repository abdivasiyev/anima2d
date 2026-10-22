{- | The tools an agent draws with, on one project: what each takes, and what it does. The
MCP server offers them over a project store (adding the project's id); the desktop's
assistant runs them on the open project, each change one undoable edit. Either way the
arguments are parsed here and every item by its own parser, so an agent cannot write
what the editor would refuse.
-}
module Anima.Tools
    ( ToolSpec (..)
    , ToolCall (..)
    , projectTools
    , projectTool
    , toolSchema
    , instructions
    , itemFormat

      -- * Reading arguments
    , arg
    , optional
    , asText
    , asInt
    , asBool
    , asList
    , traverseIndexed
    , str
    , int
    , bool
    , arr
    )
where

import Data.Aeson (Value (..), object, (.=))
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as Text

import Anima.Types.Parse (Parse (..))
import Anima.Types.Project
    ( Item
    , Project
    , ProjectView (..)
    , addItemsAt
    , addLayer
    , addLightLayer
    , drawingAt
    , duration
    , insertDrawing
    , layersSummary
    , pickAnyLayer
    , pickLayer
    , replaceItemsAt
    , restyleLayer
    , setCamera
    , viewProject
    )

-- | A tool as an agent is told of it: its name, what it does, and its arguments' schema.
data ToolSpec = ToolSpec {toolName :: Text, description :: Text, properties :: [(Text, Value)], required :: [Text]}

-- | What a tool does to one project: reads something from it, or changes it.
data ToolCall
    = Reads (Project -> Either Text Value)
    | Changes (Project -> Either Text Project)

-- | The arguments' JSON schema.
toolSchema :: ToolSpec -> Value
toolSchema t = object ["type" .= ("object" :: Text), "properties" .= object [Key.fromText k .= v | (k, v) <- t.properties], "required" .= t.required]

instructions :: Text
instructions =
    "anima2D: 2D frame-by-frame animation. A project is a canvas, a frame rate and layers stacked bottom \
    \to top; a drawing layer is a run of drawings, each held for some frames and holding items drawn in \
    \order. A background layer holds its one drawing to the end. Call get_project for the layers and \
    \item_format for the exact JSON of strokes, shapes, curves, fills, scatter brushes, text and pictures, and set_layer to move whole layers with keyframes. Coordinates \
    \are canvas pixels from the top-left. Drawings are numbered from 1; leave out `layer` to use the \
    \topmost drawing layer."

-- | The tools on one project, without a project id.
projectTools :: [ToolSpec]
projectTools =
    [ ToolSpec "get_project" "The project's canvas, frame rate, length and layers (ids, names, number of drawings)." [] []
    , ToolSpec
        "add_layer"
        "Add a drawing layer on top, or with background=true a layer at the bottom whose one drawing is held to the end."
        [("name", str "1 to 40 characters"), ("background", bool "A background layer (default false)")]
        ["name"]
    , ToolSpec "get_drawing" "One drawing's hold, mark and items, as JSON." [("layer", str layerHelp), ("drawing", int "Drawing number, from 1")] ["drawing"]
    , ToolSpec
        "add_items"
        "Draw items on top of a drawing. See item_format for the item JSON. All items are checked first; on any rejection nothing is added."
        [("layer", str layerHelp), ("drawing", int "Drawing number, from 1"), ("items", arr "Items, in drawing order")]
        ["drawing", "items"]
    , ToolSpec
        "replace_items"
        "Replace every item of a drawing (an empty list clears it)."
        [("layer", str layerHelp), ("drawing", int "Drawing number, from 1"), ("items", arr "Items, in drawing order")]
        ["drawing", "items"]
    , ToolSpec
        "add_drawing"
        "Insert a drawing after drawing `after` (0 puts it first): blank, or a copy of drawing `after` to change for the next pose."
        [ ("layer", str layerHelp)
        , ("after", int "Insert after this drawing; 0 for the start")
        , ("copy", bool "Start from a copy of drawing `after`")
        , ("hold", int "Frames to hold it for, 1 to 24 (default 1)")
        ]
        ["after"]
    , ToolSpec
        "set_layer"
        "Change a layer's blend, clipping, motion (keyframes that move, scale, turn and fade the whole layer), depth (parallax), effects (drop shadow, rim light) or, for a light layer, its light. See item_format for the JSON. Leave a key out to keep it; null clears motion or effects."
        [ ("layer", str layerHelp)
        , ("blend", str "normal | multiply | screen | overlay | add | darken | lighten")
        , ("clip", bool "Clip to the layer below")
        , ("motion", object ["type" .= ["object" :: Text, "null"], "description" .= ("{pivot:[x,y], keys:[{frame, position:[dx,dy], scale, rotation, opacity, ease}]}" :: Text)])
        , ("depth", object ["type" .= ("number" :: Text), "description" .= ("0.2 to 20; 1 is the canvas plane, more is farther (moves less when the camera moves)" :: Text)])
        , ("effects", object ["type" .= ["object" :: Text, "null"], "description" .= ("{shadow?:{color, opacity, offset:[dx,dy], blur}, rim?:{color, opacity, width, angle}}" :: Text)])
        , ("light", object ["type" .= ("object" :: Text), "description" .= ("For a light layer: {shape, color, intensity, center, radius, angle, softness}" :: Text)])
        ]
        []
    , ToolSpec
        "add_light"
        "Add a light layer on top: a soft pool (radial) or band (linear) of coloured light added to everything under it. Move or animate it with set_layer motion; clip it to the layer below to light one character."
        [("name", str "1 to 40 characters"), ("light", object ["type" .= ("object" :: Text), "description" .= ("{shape: radial|linear, color, intensity 0-1, center:[x,y], radius, angle, softness 0-1}" :: Text)])]
        ["name", "light"]
    , ToolSpec
        "set_camera"
        "Replace the camera's keys (an empty list removes the camera). Each key frames the shot and sets shake, depth of field and a tint, easing into the next. See item_format."
        [("keys", arr "Camera keys in frame order")]
        ["keys"]
    , ToolSpec "item_format" "The JSON for every kind of item and for layer motion, with examples. Read this before drawing." [] []
    ]
    where
        layerHelp = "Layer id from get_project; leave out for the topmost drawing layer"

{- | Tool `name` with these arguments, on one project: Nothing for a name not among
'projectTools', a rejection for arguments that do not parse.
-}
projectTool :: Text -> Value -> Maybe (Either Text ToolCall)
projectTool name args = case name of
    "get_project" -> Just . withArgs $ \_ -> Right (Reads (Right . projectJson))
    "item_format" -> Just (Right (Reads (const (Right (String itemFormat)))))
    "get_drawing" -> Just . withArgs $ \o -> do
        want <- optional o "layer" (fmap Just . asText) Nothing
        n <- arg o "drawing" asInt
        pure (Reads (\p -> pickLayer want p >>= \li -> drawingAt li n p))
    "add_layer" -> Just . withArgs $ \o -> do
        nm <- arg o "name" asText
        bg <- optional o "background" asBool False
        pure (Changes (addLayer nm bg))
    "add_items" -> Just (items addItemsAt)
    "replace_items" -> Just (items replaceItemsAt)
    "add_drawing" -> Just . withArgs $ \o -> do
        want <- optional o "layer" (fmap Just . asText) Nothing
        after <- arg o "after" asInt
        copy <- optional o "copy" asBool False
        holdFor <- optional o "hold" asInt 1
        pure (Changes (\p -> pickLayer want p >>= \li -> insertDrawing li after copy holdFor p))
    "set_layer" -> Just . withArgs $ \o -> do
        want <- optional o "layer" (fmap Just . asText) Nothing
        pure (Changes (\p -> pickAnyLayer want p >>= \li -> restyleLayer li o p))
    "add_light" -> Just . withArgs $ \o -> do
        nm <- arg o "name" asText
        li <- arg o "light" Right
        pure (Changes (addLightLayer nm li))
    "set_camera" -> Just . withArgs $ \o -> do
        keys <- arg o "keys" Right
        pure (Changes (setCamera keys))
    _ -> Nothing
    where
        items f = withArgs $ \o -> do
            want <- optional o "layer" (fmap Just . asText) Nothing
            n <- arg o "drawing" asInt
            parsed <- arg o "items" (asList >=>. traverseIndexed (parse @Item))
            pure (Changes (\p -> pickLayer want p >>= \li -> f li n parsed p))
        withArgs k = case args of
            Object o -> k o
            Null -> k mempty
            _ -> Left "expected arguments as an object"

-- | What get_project tells: the project's name, canvas, rate, length and layers.
projectJson :: Project -> Value
projectJson p =
    let
        v = viewProject p
    in
        object ["name" .= v.title, "width" .= v.width, "height" .= v.height, "fps" .= v.rate, "frames" .= duration p, "layers" .= layersSummary p]

itemFormat :: Text
itemFormat =
    Text.unlines
        [ "Every item is a JSON object with a \"kind\". Colours are \"#rrggbb\". Sizes are line widths in canvas pixels (0.5 to 200)."
        , "Opacity is 0.05 to 1. \"layer\" is \"line\" (linework, drawn on top) or \"color\" (paint and fills, always drawn under the lines)."
        , "Points are [x, y, pressure] with pressure 0 to 1; positions [x, y]. Rotation is radians, -pi to pi."
        , ""
        , "stroke (freehand line; brush pen | pencil | marker | paint):"
        , "  {\"kind\":\"stroke\",\"layer\":\"line\",\"brush\":\"pen\",\"color\":\"#1c1c1e\",\"size\":6,\"opacity\":1,\"points\":[[100,100,0.5],[140,90,0.7],[180,110,0.5]]}"
        , "  Give points every few pixels for a smooth line; pressure thins and thickens it."
        , "shape (line | rect | ellipse | triangle, by centre, size and turn; fill is a colour or null):"
        , "  {\"kind\":\"shape\",\"layer\":\"line\",\"shape\":\"ellipse\",\"color\":\"#1c1c1e\",\"size\":4,\"opacity\":1,\"fill\":\"#f3c843\",\"center\":[960,540],\"width\":300,\"height\":200,\"rotation\":0}"
        , "  A line's width is its length and height is 0; rotation gives its direction."
        , "curve (quadratic Bezier: start, control point, end):"
        , "  {\"kind\":\"curve\",\"layer\":\"line\",\"color\":\"#1c1c1e\",\"size\":5,\"opacity\":1,\"points\":[[100,500],[300,300],[500,500]]}"
        , "fill (filled outlines, even-odd so inner outlines are holes; at least 3 points each):"
        , "  {\"kind\":\"fill\",\"layer\":\"color\",\"color\":\"#9fc7e8\",\"opacity\":1,\"polygons\":[[[0,0],[1920,0],[1920,600],[0,600]]]}"
        , "scatter (stamps along a path; brush grass | birds | stars | leaves | flowers | spray; seed fixes the randomness):"
        , "  {\"kind\":\"scatter\",\"layer\":\"line\",\"brush\":\"grass\",\"color\":\"#4f9a55\",\"size\":40,\"opacity\":1,\"seed\":42,\"points\":[[0,1000,0.6],[1920,1010,0.6]]}"
        , "erase (pixel eraser path; cuts everything drawn before it):"
        , "  {\"kind\":\"erase\",\"size\":20,\"points\":[[500,500,1],[520,510,1]]}"
        , "text (lines split by \\n; font is sans-serif | serif | monospace | cursive | system-ui or a family from the project's fonts; style normal | bold | italic | bold-italic; size 4 to 2000; align left | center | right; centre of the block):"
        , "  {\"kind\":\"text\",\"layer\":\"line\",\"text\":\"Hello\",\"font\":\"sans-serif\",\"style\":\"bold\",\"size\":96,\"align\":\"center\",\"color\":\"#1c1c1e\",\"opacity\":1,\"center\":[960,200],\"rotation\":0}"
        , "image (an uploaded asset by its centre, drawn size and turn; flip mirrors it):"
        , "  {\"kind\":\"image\",\"layer\":\"color\",\"asset\":\"<asset id>\",\"opacity\":1,\"center\":[960,540],\"width\":1920,\"height\":1080,\"rotation\":0,\"flip\":false}"
        , ""
        , "Depth (set_layer depth): 1 is the canvas; 3 a background, 8 far hills, 0.6 a foreground, 0.3 something near. Shows when the camera moves (parallax)."
        , "Effects (set_layer effects): {\"shadow\":{\"color\":\"#1c1c1e\",\"opacity\":0.35,\"offset\":[14,22],\"blur\":14},\"rim\":{\"color\":\"#ffd27a\",\"opacity\":0.85,\"width\":6,\"angle\":-2.36}} — angle is where the light comes from (radians, 0 from the right, -pi/2 from above)."
        , "Light (add_light): {\"shape\":\"radial\",\"color\":\"#ffd8a0\",\"intensity\":0.6,\"center\":[1300,300],\"radius\":700,\"angle\":0,\"softness\":0.85}; set_layer blend multiply makes it a shadow light."
        , "Camera keys (set_camera): {\"frame\":0,\"center\":[960,540],\"zoom\":1,\"rotation\":0,\"ease\":\"inOut\",\"shake\":0,\"focus\":1,\"aperture\":0,\"tint\":\"#ff9a5c\",\"tintAmount\":0.25}; zoom 0.1 to 10, shake pixels 0 to 200, focus the depth in focus, aperture blur per unit of depth away (0 to 50), tint null for none."
        , "Layer motion (set_layer): keys in frame order, one per frame, frames from 0; position is an offset from where the drawings are; scale 0.01 to 100 and rotation turn about the pivot; opacity 0 to 1; ease linear | in | out | inOut eases into the next key."
        , "  {\"pivot\":[960,540],\"keys\":[{\"frame\":0,\"position\":[0,0],\"scale\":1,\"rotation\":0,\"opacity\":1,\"ease\":\"inOut\"},{\"frame\":24,\"position\":[400,-100],\"scale\":1.5,\"rotation\":0.3,\"opacity\":1,\"ease\":\"linear\"}]}"
        , ""
        , "Animate by adding drawings (add_drawing with copy=true) and changing each a little: a few pixels of movement per drawing reads as smooth motion at 12 to 24 fps."
        ]

-- Argument reading --------------------------------------------------------------------------

(>=>.) :: (a -> Either Text b) -> (b -> Either Text c) -> a -> Either Text c
(>=>.) f g x = f x >>= g

arg :: KeyMap.KeyMap Value -> Text -> (Value -> Either Text a) -> Either Text a
arg o k p = either (Left . ((k <> ": ") <>)) Right (p (fromMaybe Null (KeyMap.lookup (Key.fromText k) o)))

optional :: KeyMap.KeyMap Value -> Text -> (Value -> Either Text a) -> a -> Either Text a
optional o k p fallback = case KeyMap.lookup (Key.fromText k) o of
    Nothing -> Right fallback
    Just Null -> Right fallback
    Just v -> either (Left . ((k <> ": ") <>)) Right (p v)

asText :: Value -> Either Text Text
asText = \case
    String t -> Right t
    _ -> Left "expected text"

asInt :: Value -> Either Text Int
asInt = \case
    Number n | Just i <- toBoundedInt n -> Right i
    _ -> Left "expected a whole number"
    where
        toBoundedInt n = let i = round n :: Int in if fromIntegral i == n then Just i else Nothing

asBool :: Value -> Either Text Bool
asBool = \case
    Bool b -> Right b
    _ -> Left "expected true or false"

asList :: Value -> Either Text [Value]
asList = \case
    Array xs -> Right (toList xs)
    _ -> Left "expected an array"

traverseIndexed :: (Value -> Either Text a) -> [Value] -> Either Text [a]
traverseIndexed f = traverse (\(i, v) -> either (Left . ((Text.pack (show i) <> ": ") <>)) Right (f v)) . zip [0 :: Int ..]

str, int, bool, arr :: Text -> Value
str d = object ["type" .= ("string" :: Text), "description" .= d]
int d = object ["type" .= ("integer" :: Text), "description" .= d]
bool d = object ["type" .= ("boolean" :: Text), "description" .= d]
arr d = object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("object" :: Text)], "description" .= d]
