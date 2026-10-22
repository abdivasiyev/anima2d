{- | Where projects live, seen the same way by the window whichever it is: this computer (the
server's own store, in process, in a folder) or a self-hosted anima2D server (its HTTP
API, with any headers its sign-in gate wants, such as an access token).

Everything read back is parsed by the library's parsers; nothing is trusted because it
came from our own server.
-}
module Anima.Desktop.Backend
    ( Backend (..)
    , Saved (..)
    , localBackend
    , remoteBackend
    )
where

import Anima.Store (ProjectStore (..), StoreError (..), fileStore)
import Anima.Types.Parse (Parse (..))
import Anima.Types.Project (Project, ProjectId, ProjectSummary, Revision, projectId, summaryRevision)
import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS qualified as Tls
import Network.HTTP.Types qualified as HttpTypes

-- | What a save came to.
data Saved = Saved Revision | Stale Revision | Failed Text

data Backend = Backend
    { describe :: Text
    , list :: IO (Either Text [ProjectSummary])
    , load :: ProjectId -> IO (Either Text (Project, Revision))
    , save :: Maybe Revision -> Project -> IO Saved
    , changes :: Maybe Int -> IO (Maybe Int)
    -- ^ Wait for news (a save anywhere after save number n); the latest number, or Nothing offline.
    , picture :: Text -> IO (Maybe FilePath)
    -- ^ An asset as a local file (fetched and kept, for a server).
    }

-- | Projects in a folder on this computer, through the server's own store.
localBackend :: FilePath -> IO Backend
localBackend dir = do
    store <- fileStore dir
    let
        failure = Text.pack . showError
    pure
        Backend
            { describe = "This computer · " <> Text.pack dir
            , list = either (Left . failure) Right <$> store.list
            , load =
                fmap
                    ( either
                        (Left . failure)
                        (\(p, s) -> Right (p, summaryRevision s))
                    )
                    . store.load
            , save = \base p ->
                store.save base p >>= \case
                    Right s -> pure (Saved (summaryRevision s))
                    Left (Conflict now) -> pure (Stale now)
                    Left e -> pure (Failed (failure e))
            , changes = \after -> Just <$> store.changes (fromMaybe (-1) after)
            , picture = \asset -> do
                let
                    path = dir </> "assets" </> Text.unpack asset
                found <- doesFileExist path
                pure (if found then Just path else Nothing)
            }

showError :: StoreError -> String
showError = \case
    NotFound -> "no such project"
    Conflict _ -> "someone else saved first"
    Corrupt why -> "a project file could not be read: " <> Text.unpack why
    Unavailable why -> "the projects folder could not be used: " <> Text.unpack why

{- | A self-hosted server at `base` (e.g. https://anim.example.com), with extra request
headers (an access token for the gate in front of it); assets are kept in `cacheDir`.
-}
remoteBackend :: Text -> [(Text, Text)] -> FilePath -> IO Backend
remoteBackend base headers cacheDir = do
    manager <- Http.newManager Tls.tlsManagerSettings
    let
        url path = Text.unpack (Text.dropWhileEnd (== '/') base) <> path
        extra = [(CI.mk (Text.encodeUtf8 k), Text.encodeUtf8 v) | (k, v) <- headers]
        call :: ByteString -> String -> [(HttpTypes.HeaderName, ByteString)] -> Maybe LBS.ByteString -> Int -> IO (Either Text (Http.Response LBS.ByteString))
        call method path hs body seconds = do
            r <- try $ do
                req0 <- Http.parseRequest (url path)
                let
                    req =
                        req0
                            { Http.method = method
                            , Http.requestHeaders = extra <> hs
                            , Http.requestBody = maybe (Http.requestBody req0) Http.RequestBodyLBS body
                            , Http.responseTimeout = Http.responseTimeoutMicro (seconds * 1000000)
                            , Http.redirectCount = 0
                            }
                Http.httpLbs req manager
            pure $ case r of
                Left (e :: SomeException) -> Left ("the server is out of reach (" <> Text.take 120 (Text.pack (show e)) <> ")")
                Right res
                    | HttpTypes.statusCode (Http.responseStatus res) `elem` [301, 302, 303, 307, 401] -> Left "the server asks to sign in: give the access headers in Connection"
                    | otherwise -> Right res
        status = HttpTypes.statusCode . Http.responseStatus
        decoded :: (Aeson.FromJSON a) => Http.Response LBS.ByteString -> Either Text a
        decoded res
            | status res /= 200 = Left ("the server answered " <> Text.pack (show (status res)))
            | otherwise = either (Left . Text.pack) Right (Aeson.eitherDecode (Http.responseBody res))
        revisionOf res =
            lookup "X-Revision" (Http.responseHeaders res) >>= \v -> case reads (BS8.unpack v) of
                [(n, "")] -> either (const Nothing) Just (parse (n :: Int))
                _ -> Nothing
    pure
        Backend
            { describe = "Server · " <> base
            , list = (>>= decoded) <$> call "GET" "/api/projects" [] Nothing 15
            , load = \pid -> do
                r <- call "GET" ("/api/projects/" <> Text.unpack (unparse pid)) [] Nothing 30
                pure
                    $ r >>= \res -> do
                        p <- decoded res
                        rev <- maybe (Left "the server sent no revision") Right (revisionOf res)
                        pure (p, rev)
            , save = \base' p -> do
                let
                    baseHeader = maybe [] (\b -> [("X-Base-Revision", BS8.pack (show (unparse b)))]) base'
                r <- call "PUT" ("/api/projects/" <> Text.unpack (unparse (projectId p))) (("Content-Type", "application/json") : baseHeader) (Just (Aeson.encode p)) 30
                pure $ case r of
                    Left why -> Failed why
                    Right res
                        | status res == 409 -> case Aeson.decode (Http.responseBody res) of
                            Just (Aeson.Object o) | Just v <- KeyMap.lookup "revision" o, Aeson.Success rev <- Aeson.fromJSON v -> Stale rev
                            _ -> Failed "the server reported a conflict it did not explain"
                        | otherwise -> either Failed (Saved . summaryRevision) (decoded res)
            , changes = \after -> do
                r <- call "GET" ("/api/changes" <> maybe "" (\n -> "?after=" <> show n) after) [] Nothing 35
                pure $ case r >>= decoded of
                    Right (Aeson.Object o) | Just (Aeson.Number n) <- KeyMap.lookup "seq" o -> Just (round n)
                    _ -> Nothing
            , picture = \asset -> do
                let
                    path = cacheDir </> Text.unpack asset
                have <- doesFileExist path
                if have
                    then pure (Just path)
                    else do
                        r <- call "GET" ("/api/assets/" <> Text.unpack asset) [] Nothing 60
                        case r of
                            Right res | status res == 200 -> LBS.writeFile path (Http.responseBody res) >> pure (Just path)
                            _ -> pure Nothing
            }
