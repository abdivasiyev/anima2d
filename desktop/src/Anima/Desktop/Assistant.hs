{- | The assistant's model: a conversation kept in the provider's own wire form, one turn at
a time. Two providers: Anthropic's Messages API, and any server speaking OpenAI's chat
completions (OpenAI itself, or a local model through Ollama, LM Studio, llama.cpp…).
Requests and replies are built and read by pure functions, so the tests can check both
shapes without a network.
-}
module Anima.Desktop.Assistant
    ( Provider (..)
    , Settings (..)
    , providerName
    , defaultSettings
    , parseSettings
    , settingsValue
    , Model (..)
    , Reply (..)
    , Stop (..)
    , ToolUse (..)
    , ToolOutcome (..)
    , connectModel
    , loadSettings
    , saveSettings

      -- * Wire shapes (pure, for the tests)
    , anthropicRequest
    , anthropicReply
    , anthropicResults
    , openAIRequest
    , openAIReply
    , openAIResults
    )
where

import Anima.Tools (ToolSpec (..), toolSchema)
import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), object, (.=))
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS qualified as Tls
import Network.HTTP.Types qualified as HttpTypes

data Provider = Anthropic | OpenAICompatible
    deriving stock (Eq, Show)

providerName :: Provider -> Text
providerName = \case
    Anthropic -> "anthropic"
    OpenAICompatible -> "openai"

-- | Which model, where. The key is kept apart, in the system's keychain.
data Settings = Settings {provider :: Provider, url :: Text, model :: Text}
    deriving stock (Eq, Show)

defaultSettings :: Provider -> Settings
defaultSettings = \case
    Anthropic -> Settings Anthropic "https://api.anthropic.com" "claude-opus-5-5"
    OpenAICompatible -> Settings OpenAICompatible "http://localhost:11434/v1" ""

parseSettings :: Value -> Either Text Settings
parseSettings = \case
    Object o -> do
        p <- case KeyMap.lookup "provider" o of
            Just (String "anthropic") -> Right Anthropic
            Just (String "openai") -> Right OpenAICompatible
            _ -> Left "provider: expected \"anthropic\" or \"openai\""
        u <- case KeyMap.lookup "url" o of
            Just (String t) | "https://" `Text.isPrefixOf` t || "http://" `Text.isPrefixOf` t -> Right (Text.dropWhileEnd (== '/') (Text.strip t))
            _ -> Left "url: expected an address starting with https:// or http://"
        m <- case KeyMap.lookup "model" o of
            Just (String t) | not (Text.null (Text.strip t)) && Text.length t <= 200 -> Right (Text.strip t)
            _ -> Left "model: expected a model name"
        Right (Settings p u m)
    _ -> Left "expected an object"

settingsValue :: Settings -> Value
settingsValue s = object ["provider" .= providerName s.provider, "url" .= s.url, "model" .= s.model]

settingsFile :: IO FilePath
settingsFile = do
    dir <- getXdgDirectory XdgConfig "anima2d-desktop"
    createDirectoryIfMissing True dir
    pure (dir </> "assistant.json")

-- | The saved connection, parsed back; a file that does not parse is as good as none.
loadSettings :: IO (Maybe Settings)
loadSettings = do
    path <- settingsFile
    raw <- try (LBS.readFile path)
    pure $ case raw of
        Left (_ :: SomeException) -> Nothing
        Right bytes -> Aeson.decode bytes >>= either (const Nothing) Just . parseSettings

saveSettings :: Settings -> IO ()
saveSettings s = settingsFile >>= \path -> LBS.writeFile path (Aeson.encode (settingsValue s))

data ToolUse = ToolUse {callId :: Text, name :: Text, input :: Value}
    deriving stock (Eq, Show)

-- | What running a tool gave back: words, maybe a picture (PNG), and whether it failed.
data ToolOutcome = ToolOutcome {callId :: Text, text :: Text, image :: Maybe ByteString, failed :: Bool}

data Stop
    = -- | The model has answered.
      Done
    | -- | It asks for tools to run; their outcomes go back in the next turn.
      WantsTools
    | -- | It ran out of room mid-answer.
      CutOff
    | -- | It declined (the provider's own words, if any).
      Declined Text
    deriving stock (Eq, Show)

-- | One reply: what it said, the tools it asks for, why it stopped, and the message to keep.
data Reply = Reply {said :: Text, calls :: [ToolUse], stop :: Stop, keep :: Value}

-- | A model to talk to, whatever the provider.
data Model = Model
    { label :: Text
    , ask :: Text -> [ToolSpec] -> [Value] -> IO (Either Text Reply)
    -- ^ The next reply, from the system prompt, the tools and the conversation so far.
    , userTurn :: Text -> [Value]
    , resultTurn :: [ToolOutcome] -> [Value]
    }

-- | A model over HTTP, with the key (empty for a local server that needs none).
connectModel :: Settings -> Text -> IO Model
connectModel s key = do
    manager <- Tls.newTlsManager
    let
        post path headers body = do
            r <- try $ do
                req0 <- Http.parseRequest (Text.unpack (s.url <> path))
                let
                    req =
                        req0
                            { Http.method = "POST"
                            , Http.requestHeaders = ("Content-Type", "application/json") : headers
                            , Http.requestBody = Http.RequestBodyLBS (Aeson.encode body)
                            , Http.responseTimeout = Http.responseTimeoutMicro (600 * 1000000)
                            }
                Http.httpLbs req manager
            pure $ case r of
                Left (e :: SomeException) -> Left ("the model is out of reach (" <> Text.take 160 (Text.pack (show e)) <> ")")
                Right res
                    | HttpTypes.statusCode (Http.responseStatus res) /= 200 -> Left ("the model answered " <> Text.pack (show (HttpTypes.statusCode (Http.responseStatus res))) <> ": " <> errorText (Http.responseBody res))
                    | otherwise -> maybe (Left "the model's answer was not JSON") Right (Aeson.decode (Http.responseBody res))
    pure $ case s.provider of
        Anthropic ->
            Model
                { label = s.model
                , ask = \system tools history -> fmap (>>= anthropicReply) (post "/v1/messages" (anthropicHeaders key) (anthropicRequest s.model system tools history))
                , userTurn = \t -> [object ["role" .= ("user" :: Text), "content" .= t]]
                , resultTurn = anthropicResults
                }
        OpenAICompatible ->
            Model
                { label = s.model
                , ask = \system tools history -> fmap (>>= openAIReply) (post "/chat/completions" [("Authorization", "Bearer " <> Text.encodeUtf8 key) | not (Text.null key)] (openAIRequest s.model system tools history))
                , userTurn = \t -> [object ["role" .= ("user" :: Text), "content" .= t]]
                , resultTurn = openAIResults
                }

anthropicHeaders :: Text -> [HttpTypes.Header]
anthropicHeaders key =
    [ ("x-api-key", Text.encodeUtf8 key)
    , ("anthropic-version", "2023-06-01")
    , -- A declined request is retried on Anthropic's recommended model for its category.
      ("anthropic-beta", "server-side-fallback-2026-07-01")
    ]

-- | The provider's error message, when the body says one.
errorText :: LBS.ByteString -> Text
errorText body = case Aeson.decode body of
    Just (Object o)
        | Just (Object e) <- KeyMap.lookup "error" o
        , Just (String m) <- KeyMap.lookup "message" e ->
            Text.take 300 m
    _ -> Text.take 300 (Text.decodeUtf8Lenient (LBS.toStrict body))

-- Anthropic -------------------------------------------------------------------------------

anthropicRequest :: Text -> Text -> [ToolSpec] -> [Value] -> Value
anthropicRequest m system tools history =
    object
        [ "model" .= m
        , "max_tokens" .= (16000 :: Int)
        , "system" .= system
        , "tools" .= [object ["name" .= t.toolName, "description" .= t.description, "input_schema" .= toolSchema t] | t <- tools]
        , "messages" .= history
        , "output_config" .= object ["effort" .= ("high" :: Text)]
        , "fallbacks" .= ("default" :: Text)
        ]

{- | A Messages API reply. Its content goes back unchanged as the assistant's turn (thinking
blocks included), so the next request continues the same conversation.
-}
anthropicReply :: Value -> Either Text Reply
anthropicReply = \case
    Object o -> do
        blocks <- case KeyMap.lookup "content" o of
            Just (Array bs) -> Right (toList bs)
            _ -> Left "the reply had no content"
        let
            texts = [t | Object b <- blocks, Just (String "text") <- [KeyMap.lookup "type" b], Just (String t) <- [KeyMap.lookup "text" b]]
            uses =
                [ ToolUse i n (fromMaybe (Object mempty) (KeyMap.lookup "input" b))
                | Object b <- blocks
                , Just (String "tool_use") <- [KeyMap.lookup "type" b]
                , Just (String i) <- [KeyMap.lookup "id" b]
                , Just (String n) <- [KeyMap.lookup "name" b]
                ]
            why = case KeyMap.lookup "stop_details" o of
                Just (Object d) | Just (String e) <- KeyMap.lookup "explanation" d -> e
                _ -> ""
            stopped = case KeyMap.lookup "stop_reason" o of
                Just (String "tool_use") -> WantsTools
                Just (String "max_tokens") -> CutOff
                Just (String "refusal") -> Declined why
                _ -> Done
        Right
            Reply
                { said = Text.strip (Text.intercalate "\n\n" texts)
                , calls = if stopped == WantsTools then uses else []
                , stop = stopped
                , keep = object ["role" .= ("assistant" :: Text), "content" .= blocks]
                }
    _ -> Left "the reply was not an object"

-- | Every tool's outcome in one user turn, pictures as image blocks.
anthropicResults :: [ToolOutcome] -> [Value]
anthropicResults outs =
    [ object
        [ "role" .= ("user" :: Text)
        , "content"
            .= [ object
                    [ "type" .= ("tool_result" :: Text)
                    , "tool_use_id" .= r.callId
                    , "is_error" .= r.failed
                    , "content"
                        .= ( [object ["type" .= ("image" :: Text), "source" .= object ["type" .= ("base64" :: Text), "media_type" .= ("image/png" :: Text), "data" .= Text.decodeUtf8 (Base64.encode png)]] | Just png <- [r.image]]
                                <> [object ["type" .= ("text" :: Text), "text" .= r.text]]
                           )
                    ]
               | r <- outs
               ]
        ]
    | not (null outs)
    ]

-- OpenAI-compatible --------------------------------------------------------------------------

openAIRequest :: Text -> Text -> [ToolSpec] -> [Value] -> Value
openAIRequest m system tools history =
    object
        [ "model" .= m
        , "messages" .= (object ["role" .= ("system" :: Text), "content" .= system] : history)
        , "tools" .= [object ["type" .= ("function" :: Text), "function" .= object ["name" .= t.toolName, "description" .= t.description, "parameters" .= toolSchema t]] | t <- tools]
        ]

openAIReply :: Value -> Either Text Reply
openAIReply = \case
    Object o -> case KeyMap.lookup "choices" o of
        Just (Array cs)
            | Object c : _ <- toList cs
            , Just (Object msg) <- KeyMap.lookup "message" c -> do
                let
                    content = case KeyMap.lookup "content" msg of
                        Just (String t) -> t
                        _ -> ""
                    uses = case KeyMap.lookup "tool_calls" msg of
                        Just (Array tcs) -> mapMaybe toolCall (toList tcs)
                        _ -> []
                    stopped = case KeyMap.lookup "finish_reason" c of
                        Just (String "length") -> CutOff
                        Just (String "content_filter") -> Declined ""
                        _ | not (null uses) -> WantsTools
                        _ -> Done
                Right Reply {said = Text.strip content, calls = uses, stop = stopped, keep = Object msg}
        _ -> Left "the reply had no choices"
    _ -> Left "the reply was not an object"
    where
        toolCall = \case
            Object t
                | Just (String i) <- KeyMap.lookup "id" t
                , Just (Object f) <- KeyMap.lookup "function" t
                , Just (String n) <- KeyMap.lookup "name" f ->
                    let
                        args = case KeyMap.lookup "arguments" f of
                            Just (String raw) -> fromMaybe (Object mempty) (Aeson.decodeStrict (Text.encodeUtf8 raw))
                            Just v@(Object _) -> v
                            _ -> Object mempty
                    in
                        Just (ToolUse i n args)
            _ -> Nothing

{- | One tool message per outcome; a picture (which tool messages cannot carry) follows as a
user message with the image.
-}
openAIResults :: [ToolOutcome] -> [Value]
openAIResults outs =
    [object ["role" .= ("tool" :: Text), "tool_call_id" .= r.callId, "content" .= (if r.failed then "Error: " <> r.text else r.text)] | r <- outs]
        <> [ object
                [ "role" .= ("user" :: Text)
                , "content"
                    .= [ object ["type" .= ("text" :: Text), "text" .= ("The picture from the " <> r.callId <> " call." :: Text)]
                       , object ["type" .= ("image_url" :: Text), "image_url" .= object ["url" .= ("data:image/png;base64," <> Text.decodeUtf8 (Base64.encode png))]]
                       ]
                ]
           | r <- outs
           , Just png <- [r.image]
           ]
