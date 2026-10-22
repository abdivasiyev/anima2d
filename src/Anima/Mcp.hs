{- | anima2D as an MCP server, so an agent can draw into a project.

Streamable HTTP, the simple half of it: each JSON-RPC request is a @POST /mcp@ answered
with one JSON body; notifications get 202. That is all the methods here need — nothing
streams.

Every item an agent sends is parsed by the same 'Parse' instance the editor's saves go
through, so a malformed item comes back as the exact rejection, and nothing invalid can
reach a project. Writes go through 'modifyProject', so they are revisioned like the
editor's saves: an editor that has the project open sees the change on its next poll.

Published at @https://mcp.<domain>/anima2d/mcp@ behind a sign-in gate (server
@the host configuration@); this listener is loopback only.
-}
module Anima.Mcp
    ( mcpApp
    , writeTools
    )
where

import Data.Aeson (Value (..), object, (.=))
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

import Anima.Store (ProjectStore (..), StoreError (..), modifyProject)
import Anima.Types.Parse (Parse (..))
import Anima.Types.Project
    ( Item
    , Project
    , ProjectSummary
    , addItemsAt
    , addLayer
    , addLightLayer
    , canvasFrom
    , drawingAt
    , duration
    , fpsFrom
    , insertDrawing
    , layersSummary
    , newProject
    , pickAnyLayer
    , pickLayer
    , replaceItemsAt
    , restyleLayer
    , setCamera
    )

-- | The tools that change a project; the server lists the same names for read-only agents.
writeTools :: [Text]
writeTools = ["create_project", "add_items", "replace_items", "add_drawing", "add_layer", "set_layer", "undo", "redo", "add_light", "set_camera"]

-- | A frame of a project drawn as a PNG: (frame from 0, width in pixels).
type Preview = Project -> Int -> Int -> IO (Either Text LBS.ByteString)

mcpApp :: ProjectStore -> IO Text -> Preview -> Wai.Application
mcpApp store newId preview req respond
    | Wai.pathInfo req /= ["mcp"] = respond (Wai.responseLBS Http.status404 [] "not found")
    | Wai.requestMethod req /= Http.methodPost = respond (Wai.responseLBS Http.status405 [("Allow", "POST")] "POST JSON-RPC to /mcp")
    | otherwise = do
        body <- Wai.strictRequestBody req
        case Aeson.eitherDecode body of
            Left _ -> respond (json Http.status400 (rpcError Null (-32700) "expected a JSON-RPC request"))
            Right (Object msg) -> case KeyMap.lookup "id" msg of
                -- A notification (initialized, cancelled): nothing to answer.
                Nothing -> respond (Wai.responseLBS Http.status202 [] "")
                Just rid -> do
                    reply <- handle store newId preview (textField "method" msg) (lookupOr (Object mempty) "params" msg)
                    respond . json Http.status200 $ case reply of
                        Right result -> object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid, "result" .= result]
                        Left (code, why) -> rpcError rid code why
            Right _ -> respond (json Http.status400 (rpcError Null (-32600) "expected a JSON-RPC object"))
    where
        json status v = Wai.responseLBS status [("Content-Type", "application/json")] (Aeson.encode v)

rpcError :: Value -> Int -> Text -> Value
rpcError rid code why = object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid, "error" .= object ["code" .= code, "message" .= why]]

lookupOr :: Value -> Text -> KeyMap.KeyMap Value -> Value
lookupOr fallback k o = fromMaybe fallback (KeyMap.lookup (Key.fromText k) o)

textField :: Text -> KeyMap.KeyMap Value -> Text
textField k o = case KeyMap.lookup (Key.fromText k) o of
    Just (String t) -> t
    _ -> ""

handle :: ProjectStore -> IO Text -> Preview -> Text -> Value -> IO (Either (Int, Text) Value)
handle store newId preview method params = case method of
    "initialize" ->
        pure . Right
            $ object
                [ "protocolVersion" .= clientVersion
                , "capabilities" .= object ["tools" .= object ["listChanged" .= False]]
                , "serverInfo" .= object ["name" .= ("anima2d" :: Text), "version" .= ("0.2.0" :: Text)]
                , "instructions" .= instructions
                ]
    "ping" -> pure (Right (object []))
    "tools/list" -> pure (Right (object ["tools" .= tools]))
    "tools/call" -> case params of
        Object p -> Right <$> callTool store newId preview (textField "name" p) (lookupOr (Object mempty) "arguments" p)
        _ -> pure (Left (-32602, "expected params with a tool name"))
    _ -> pure (Left (-32601, "unknown method"))
    where
        clientVersion = case params of
            Object p | Just (String v) <- KeyMap.lookup "protocolVersion" p -> v
            _ -> "2025-06-18"

instructions :: Text
instructions =
    "anima2D: 2D frame-by-frame animation. A project is a canvas, a frame rate and layers stacked bottom \
    \to top; a drawing layer is a run of drawings, each held for some frames and holding items drawn in \
    \order. A background layer holds its one drawing to the end. Call get_project for the layers and \
    \item_format for the exact JSON of strokes, shapes, curves, fills, scatter brushes, text and pictures, and set_layer to move whole layers with keyframes. Coordinates \
    \are canvas pixels from the top-left. Drawings are numbered from 1; leave out `layer` to use the \
    \topmost drawing layer."

-- Tools -----------------------------------------------------------------------------------

tools :: [Value]
tools =
    [ tool "list_projects" "List the projects: id, name, canvas size, frame rate and drawing count." [] []
    , tool
        "create_project"
        "Create an empty project (one blank drawing) and return its summary."
        [ ("name", str "1 to 80 characters")
        , ("width", int "Canvas width in pixels: even, 64 to 7680 (1920 for Full HD, 1080 for Shorts)")
        , ("height", int "Canvas height in pixels: even, 64 to 7680 (1080 for Full HD, 1920 for Shorts)")
        , ("fps", int "Frame rate: 12, 24, 25, 30, 48, 50 or 60")
        ]
        ["name", "width", "height", "fps"]
    , tool "get_project" "A project's canvas, frame rate, length and layers (ids, names, number of drawings)." [("project_id", str "From list_projects")] ["project_id"]
    , tool
        "add_layer"
        "Add a drawing layer on top, or with background=true a layer at the bottom whose one drawing is held to the end."
        [("project_id", str "From list_projects"), ("name", str "1 to 40 characters"), ("background", bool "A background layer (default false)")]
        ["project_id", "name"]
    , tool "get_drawing" "One drawing's hold, mark and items, as JSON." [("project_id", str "From list_projects"), ("layer", str layerHelp), ("drawing", int "Drawing number, from 1")] ["project_id", "drawing"]
    , tool
        "add_items"
        "Draw items on top of a drawing. See item_format for the item JSON. All items are checked first; on any rejection nothing is added."
        [("project_id", str "From list_projects"), ("layer", str layerHelp), ("drawing", int "Drawing number, from 1"), ("items", arr "Items, in drawing order")]
        ["project_id", "drawing", "items"]
    , tool
        "replace_items"
        "Replace every item of a drawing (an empty list clears it)."
        [("project_id", str "From list_projects"), ("layer", str layerHelp), ("drawing", int "Drawing number, from 1"), ("items", arr "Items, in drawing order")]
        ["project_id", "drawing", "items"]
    , tool
        "add_drawing"
        "Insert a drawing after drawing `after` (0 puts it first): blank, or a copy of drawing `after` to change for the next pose."
        [ ("project_id", str "From list_projects")
        , ("layer", str layerHelp)
        , ("after", int "Insert after this drawing; 0 for the start")
        , ("copy", bool "Start from a copy of drawing `after`")
        , ("hold", int "Frames to hold it for, 1 to 24 (default 1)")
        ]
        ["project_id", "after"]
    , tool
        "set_layer"
        "Change a layer's blend, clipping, motion (keyframes that move, scale, turn and fade the whole layer), depth (parallax), effects (drop shadow, rim light) or, for a light layer, its light. See item_format for the JSON. Leave a key out to keep it; null clears motion or effects."
        [ ("project_id", str "From list_projects")
        , ("layer", str layerHelp)
        , ("blend", str "normal | multiply | screen | overlay | add | darken | lighten")
        , ("clip", bool "Clip to the layer below")
        , ("motion", object ["type" .= ["object" :: Text, "null"], "description" .= ("{pivot:[x,y], keys:[{frame, position:[dx,dy], scale, rotation, opacity, ease}]}" :: Text)])
        , ("depth", object ["type" .= ("number" :: Text), "description" .= ("0.2 to 20; 1 is the canvas plane, more is farther (moves less when the camera moves)" :: Text)])
        , ("effects", object ["type" .= ["object" :: Text, "null"], "description" .= ("{shadow?:{color, opacity, offset:[dx,dy], blur}, rim?:{color, opacity, width, angle}}" :: Text)])
        , ("light", object ["type" .= ("object" :: Text), "description" .= ("For a light layer: {shape, color, intensity, center, radius, angle, softness}" :: Text)])
        ]
        ["project_id"]
    , tool "undo" "Bring back the version before the latest change to the project (yours, the editor's or another agent's), as a new save that open editors show. Repeat to go further back; up to 100 versions are kept." [("project_id", str "From list_projects")] ["project_id"]
    , tool "redo" "Take back the last undo. Any other change since then ends redo." [("project_id", str "From list_projects")] ["project_id"]
    , tool
        "preview"
        "See a frame as it exports: a PNG of the whole picture (every visible layer, through the camera). Use it to check what you drew. Drawn by the Mac's render agent, in a second or two."
        [("project_id", str "From list_projects"), ("frame", int "Frame number on the timeline, from 1 (default 1)"), ("width", int "Image width in pixels, 64 to 1920 (default 960)")]
        ["project_id"]
    , tool
        "add_light"
        "Add a light layer on top: a soft pool (radial) or band (linear) of coloured light added to everything under it. Move or animate it with set_layer motion; clip it to the layer below to light one character."
        [("project_id", str "From list_projects"), ("name", str "1 to 40 characters"), ("light", object ["type" .= ("object" :: Text), "description" .= ("{shape: radial|linear, color, intensity 0-1, center:[x,y], radius, angle, softness 0-1}" :: Text)])]
        ["project_id", "name", "light"]
    , tool
        "set_camera"
        "Replace the camera's keys (an empty list removes the camera). Each key frames the shot and sets shake, depth of field and a tint, easing into the next. See item_format."
        [("project_id", str "From list_projects"), ("keys", arr "Camera keys in frame order")]
        ["project_id", "keys"]
    , tool "item_format" "The JSON for every kind of item and for layer motion, with examples. Read this before drawing." [] []
    ]
    where
        layerHelp = "Layer id from get_project; leave out for the topmost drawing layer"
        tool :: Text -> Text -> [(Text, Value)] -> [Text] -> Value
        tool n d props req =
            object
                [ "name" .= n
                , "description" .= d
                , "inputSchema" .= object ["type" .= ("object" :: Text), "properties" .= object [Key.fromText k .= v | (k, v) <- props], "required" .= req]
                , "annotations" .= object ["readOnlyHint" .= (n `notElem` writeTools)]
                ]
        str d = object ["type" .= ("string" :: Text), "description" .= (d :: Text)]
        int d = object ["type" .= ("integer" :: Text), "description" .= (d :: Text)]
        bool d = object ["type" .= ("boolean" :: Text), "description" .= (d :: Text)]
        arr d = object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("object" :: Text)], "description" .= (d :: Text)]

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

callTool :: ProjectStore -> IO Text -> Preview -> Text -> Value -> IO Value
callTool store newId preview name args = case name of
    "list_projects" -> either storeFailure (ok . fmap summaryJson) <$> store.list
    "item_format" -> pure (textResult False itemFormat)
    "get_project" -> withArgs $ \o -> do
        pid <- arg o "project_id" (asText >=>. parse)
        pure $ do
            loaded <- store.load pid
            pure $ either storeFailure (\(p, rev) -> ok (object ["summary" .= summaryJson rev, "frames" .= duration p, "layers" .= layersSummary p])) loaded
    "add_layer" -> withArgs $ \o -> do
        pid <- arg o "project_id" (asText >=>. parse)
        nm <- arg o "name" asText
        bg <- optional o "background" asBool False
        pure (saved <$> modifyProject store pid (addLayer nm bg))
    "get_drawing" -> withArgs $ \o -> do
        pid <- arg o "project_id" (asText >=>. parse)
        want <- optional o "layer" (fmap Just . asText) Nothing
        n <- arg o "drawing" asInt
        pure $ do
            loaded <- store.load pid
            pure $ either storeFailure (\(p, _) -> either failure ok (pickLayer want p >>= \li -> drawingAt li n p)) loaded
    "preview" -> withArgs $ \o -> do
        pid <- arg o "project_id" (asText >=>. parse)
        frame <- optional o "frame" asInt 1
        width <- optional o "width" asInt 960
        pure $ do
            loaded <- store.load pid
            case loaded of
                Left e -> pure (storeFailure e)
                Right (p, _)
                    | frame < 1 || frame > duration p -> pure (failure ("expected a frame from 1 to " <> tshowInt (duration p)))
                    | width < 64 || width > 1920 -> pure (failure "expected a width from 64 to 1920 pixels")
                    | otherwise -> do
                        png <- preview p (frame - 1) width
                        pure $ case png of
                            Left why -> failure why
                            Right bytes ->
                                object
                                    [ "content"
                                        .= [ object ["type" .= ("image" :: Text), "mimeType" .= ("image/png" :: Text), "data" .= Text.decodeUtf8 (Base64.encode (LBS.toStrict bytes))]
                                           , object ["type" .= ("text" :: Text), "text" .= ("Frame " <> tshowInt frame <> " of " <> tshowInt (duration p) <> ", as exported (white paper, through the camera)." :: Text)]
                                           ]
                                    , "isError" .= False
                                    ]
    "undo" -> history "Nothing older to go back to" store.undo
    "redo" -> history "Nothing to redo: there was no undo since the last change" store.redo
    "set_layer" -> withArgs $ \o -> do
        pid <- arg o "project_id" (asText >=>. parse)
        want <- optional o "layer" (fmap Just . asText) Nothing
        pure (saved <$> modifyProject store pid (\p -> pickAnyLayer want p >>= \li -> restyleLayer li o p))
    "add_light" -> withArgs $ \o -> do
        pid <- arg o "project_id" (asText >=>. parse)
        nm <- arg o "name" asText
        li <- arg o "light" Right
        pure (saved <$> modifyProject store pid (addLightLayer nm li))
    "set_camera" -> withArgs $ \o -> do
        pid <- arg o "project_id" (asText >=>. parse)
        keys <- arg o "keys" Right
        pure (saved <$> modifyProject store pid (setCamera keys))
    "add_items" -> edit addItemsAt
    "replace_items" -> edit replaceItemsAt
    "add_drawing" -> withArgs $ \o -> do
        pid <- arg o "project_id" (asText >=>. parse)
        want <- optional o "layer" (fmap Just . asText) Nothing
        after <- arg o "after" asInt
        copy <- optional o "copy" asBool False
        holdFor <- optional o "hold" asInt 1
        pure (saved <$> modifyProject store pid (\p -> pickLayer want p >>= \li -> insertDrawing li after copy holdFor p))
    "create_project" -> withArgs $ \o -> do
        pname <- arg o "name" (asText >=>. parse)
        c <- canvasFrom (object ["width" .= KeyMap.lookup "width" o, "height" .= KeyMap.lookup "height" o])
        f <- arg o "fps" fpsFrom
        pure $ do
            raw <- newId
            case parse raw of
                Left why -> pure (failure why)
                Right pid -> either storeFailure (ok . summaryJson) <$> store.save Nothing (newProject pid pname c f)
    _ -> pure (failure "unknown tool")
    where
        edit f = withArgs $ \o -> do
            pid <- arg o "project_id" (asText >=>. parse)
            want <- optional o "layer" (fmap Just . asText) Nothing
            n <- arg o "drawing" asInt
            items <- arg o "items" (asList >=>. traverseIndexed (parse @Item))
            pure (saved <$> modifyProject store pid (\p -> pickLayer want p >>= \li -> f li n items p))
        saved = either (either storeFailure failure) (ok . summaryJson)
        history none step = withArgs $ \o -> do
            pid <- arg o "project_id" (asText >=>. parse)
            pure (either storeFailure (maybe (failure none) (ok . summaryJson)) <$> step pid)
        withArgs k = case args of
            Object o -> either (pure . failure) Prelude.id (k o)
            _ -> pure (failure "expected arguments as an object")

-- Argument reading --------------------------------------------------------------------------

(>=>.) :: (a -> Either Text b) -> (b -> Either Text c) -> a -> Either Text c
(>=>.) f g x = f x >>= g

arg :: KeyMap.KeyMap Value -> Text -> (Value -> Either Text a) -> Either Text a
arg o k p = either (Left . ((k <> ": ") <>)) Right (p (lookupOr Null k o))

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
traverseIndexed f = traverse (\(i, v) -> either (Left . ((tshow i <> ": ") <>)) Right (f v)) . zip [0 :: Int ..]

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

-- Results -----------------------------------------------------------------------------------

textResult :: Bool -> Text -> Value
textResult isError t = object ["content" .= [object ["type" .= ("text" :: Text), "text" .= t]], "isError" .= isError]

ok :: (Aeson.ToJSON a) => a -> Value
ok = textResult False . Text.decodeUtf8 . LBS.toStrict . Aeson.encode

failure :: Text -> Value
failure = textResult True

storeFailure :: StoreError -> Value
storeFailure = \case
    NotFound -> failure "no such project"
    Conflict _ -> failure "the project changed while saving; try again"
    Corrupt _ -> failure "a stored project could not be read"
    Unavailable _ -> failure "storage is unavailable"

summaryJson :: ProjectSummary -> Value
summaryJson = Aeson.toJSON

tshowInt :: Int -> Text
tshowInt = Text.pack . show
