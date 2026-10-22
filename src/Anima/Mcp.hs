{- | anima2D as an MCP server, so an agent can draw into a project.

Streamable HTTP, the simple half of it: each JSON-RPC request is a @POST /mcp@ answered
with one JSON body; notifications get 202. That is all the methods here need — nothing
streams.

Every item an agent sends is parsed by the same 'Parse' instance the editor's saves go
through, so a malformed item comes back as the exact rejection, and nothing invalid can
reach a project. Writes go through 'modifyProject', so they are revisioned like the
editor's saves: an editor that has the project open sees the change on its next poll.

This listener is loopback only; publish it behind a sign-in gate.
-}
module Anima.Mcp
    ( mcpApp
    , writeTools
    )
where

import Data.Aeson (Value (..), object, (.=))
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
import Anima.Tools (ToolCall (..), ToolSpec (..), arg, asInt, asText, int, itemFormat, optional, projectTool, projectTools, str, toolSchema)
import Anima.Types.Parse (Parse (..))
import Anima.Types.Project
    ( Project
    , ProjectSummary
    , canvasFrom
    , duration
    , fpsFrom
    , layersSummary
    , newProject
    )

import Anima.Tools qualified as Tools

-- | The tools that change a project, so a gateway can keep read-only agents off them.
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
instructions = Tools.instructions

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
    ]
        <> [ tool t.toolName t.description (if t.toolName == "item_format" then t.properties else ("project_id", projectIdArg) : t.properties) (if t.toolName == "item_format" then t.required else "project_id" : t.required)
           | t <- projectTools
           ]
        <> [ tool "undo" "Bring back the version before the latest change to the project (yours, the editor's or another agent's), as a new save that open editors show. Repeat to go further back; up to 100 versions are kept." [("project_id", projectIdArg)] ["project_id"]
           , tool "redo" "Take back the last undo. Any other change since then ends redo." [("project_id", projectIdArg)] ["project_id"]
           , tool
                "preview"
                "See a frame as it exports: a PNG of the whole picture (every visible layer, through the camera). Use it to check what you drew. Drawn by the Mac's render agent, in a second or two."
                [("project_id", projectIdArg), ("frame", int "Frame number on the timeline, from 1 (default 1)"), ("width", int "Image width in pixels, 64 to 1920 (default 960)")]
                ["project_id"]
           ]
    where
        projectIdArg = str "From list_projects"
        tool :: Text -> Text -> [(Text, Value)] -> [Text] -> Value
        tool n d props req =
            object
                [ "name" .= n
                , "description" .= d
                , "inputSchema" .= toolSchema (ToolSpec n d props req)
                , "annotations" .= object ["readOnlyHint" .= (n `notElem` writeTools)]
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
    "create_project" -> withArgs $ \o -> do
        pname <- arg o "name" (asText >=>. parse)
        c <- canvasFrom (object ["width" .= KeyMap.lookup "width" o, "height" .= KeyMap.lookup "height" o])
        f <- arg o "fps" fpsFrom
        pure $ do
            raw <- newId
            case parse raw of
                Left why -> pure (failure why)
                Right pid -> either storeFailure (ok . summaryJson) <$> store.save Nothing (newProject pid pname c f)
    _ -> case projectTool name args of
        Nothing -> pure (failure "unknown tool")
        Just (Left why) -> pure (failure why)
        Just (Right call) -> withArgs $ \o -> do
            pid <- arg o "project_id" (asText >=>. parse)
            pure $ case call of
                Reads f -> either storeFailure (\(p, _) -> either failure readResult (f p)) <$> store.load pid
                Changes f -> saved <$> modifyProject store pid f
    where
        readResult = \case
            String t -> textResult False t
            v -> ok v
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
