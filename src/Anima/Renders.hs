{-# LANGUAGE MultiWayIf #-}

{- | The render queue for a Mac: videos encoded on its GPU, for a server that has none.

A render is handed to an agent on the Mac through the folder both can see (the projects
directory, shared between them): a job is a file in
@renders/queue@ holding a snapshot of the project and the preset; the agent moves it to
@renders/working@, writes its progress to @renders/status@, and the video to
@renders/done@. No network between the two, no credentials.

Everything here is parsed: the preset by the same rules as the editor's, the agent's
status files as they are read back.
-}
module Anima.Renders (rendersApp, previewFrame) where

import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forM, when, (<=<))
import Data.Aeson (Value (..), object, (.=))
import Data.List (sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removeFile, renameFile)
import System.FilePath (takeExtension, (</>))

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

import Anima.Store (ProjectStore (..))
import Anima.Types.Parse (Parse (..))
import Anima.Types.Project (Project, projectId)

-- A preset, parsed as the editor does (web/src/domain/export.ts) -----------------------

newtype Preset = Preset Value

presetFrom :: Value -> Either Text Preset
presetFrom v = case v of
    Object o -> do
        str "id" (\s -> Text.length s >= 1 && Text.length s <= 40 && Text.all (\c -> c `elem` ['a' .. 'z'] || c `elem` ['0' .. '9'] || c == '-') s) "expected a preset id of 1 to 40 of a-z, 0-9 and '-'" o
        str "name" (\s -> let t = Text.strip s in not (Text.null t) && Text.length t <= 40) "expected a name of 1 to 40 characters" o
        dim "width" o
        dim "height" o
        case look "fps" o of
            String "project" -> Right ()
            Number n | n `elem` [12, 24, 25, 30, 48, 50, 60] -> Right ()
            _ -> Left "fps: expected a frame rate of 12, 24, 25, 30, 48, 50 or 60"
        str "codec" (`elem` ["h264", "hevc", "prores"]) "expected a codec of \"h264\", \"hevc\" or \"prores\"" o
        str "quality" (`elem` ["standard", "high", "max"]) "expected a quality of \"standard\", \"high\" or \"max\"" o
        str "fit" (`elem` ["contain", "cover"]) "expected a fit of \"contain\" or \"cover\"" o
        bool "camera" o
        bool "audio" o
        case look "background" o of
            Null -> Right ()
            String b | b `elem` ["white", "transparent"] -> Right ()
            _ -> Left "background: expected a background of \"white\" or \"transparent\""
        pure (Preset v)
    _ -> Left "expected an object"
    where
        look k o = fromMaybe Null (KeyMap.lookup (Key.fromText k) o)
        str k ok' expected o = case look k o of
            String s | ok' s -> Right ()
            String _ -> Left (k <> ": " <> expected)
            _ -> Left (k <> ": expected text")
        dim k o = case look k o of
            Number n
                | n >= 64 && n <= 7680 && fromIntegral (round n :: Int) == n && even (round n :: Int) -> Right ()
                | otherwise -> Left (k <> ": expected an even " <> k <> " from 64 to 7680 pixels")
            _ -> Left (k <> ": expected a whole number")
        bool k o = case look k o of
            Bool _ -> Right ()
            _ -> Left (k <> ": expected true or false")

-- Statuses, as the agent writes them ---------------------------------------------------

-- | What the editor's export list shows; read back from the agent's file and checked.
statusFrom :: Value -> Maybe Value
statusFrom = \case
    Object o
        | Just (String i) <- KeyMap.lookup "id" o
        , Just (String st) <- KeyMap.lookup "state" o
        , st `elem` ["queued", "rendering", "done", "failed"]
        , Just (Number pr) <- KeyMap.lookup "progress" o
        , pr >= 0 && pr <= 1 ->
            Just
                ( object
                    [ "id" .= i
                    , "state" .= st
                    , "progress" .= pr
                    , "project" .= text "project" o
                    , "preset" .= text "preset" o
                    , "created" .= text "created" o
                    , "message" .= text "message" o
                    ]
                )
    _ -> Nothing
    where
        text k o = case KeyMap.lookup k o of
            Just (String s) -> Text.take 200 s
            _ -> ""

-- The application ----------------------------------------------------------------------

{- | @POST /api/renders@ with @{"project_id", "preset"}@ queues one; @GET /api/renders@
lists them; @GET /api/renders/<id>/video@ downloads the finished one.
-}
rendersApp :: FilePath -> ProjectStore -> IO Text -> Wai.Application
rendersApp dataDir store newId req respond = do
    let
        root = dataDir </> "renders"
        queue = root </> "queue"
        status = root </> "status"
        done = root </> "done"
    mapM_ (createDirectoryIfMissing True) [queue, root </> "working", status, done]
    case (Wai.requestMethod req, Wai.pathInfo req) of
        ("POST", ["api", "renders"]) -> do
            body <- Wai.strictRequestBody req
            case Aeson.eitherDecode body of
                Right (Object o)
                    | Just (String raw) <- KeyMap.lookup "project_id" o
                    , Just pv <- KeyMap.lookup "preset" o -> case (parse raw, presetFrom pv) of
                        (Left why, _) -> respond (jsonError Http.status400 ("project_id: " <> why))
                        (_, Left why) -> respond (jsonError Http.status400 ("preset: " <> why))
                        (Right pid, Right (Preset preset)) -> do
                            loaded <- store.load pid
                            case loaded of
                                Left _ -> respond (jsonError Http.status404 "no such project")
                                Right (project, _) -> do
                                    jid <- newId
                                    now <- getCurrentTime
                                    queueJob queue status jid project preset (Text.pack (iso8601Show now))
                                    respond (Wai.responseLBS Http.status201 json (Aeson.encode (object ["id" .= jid])))
                _ -> respond (jsonError Http.status400 "expected {\"project_id\", \"preset\"}")
        ("GET", ["api", "renders"]) -> do
            names <- filter ((== ".json") . takeExtension) <$> listDirectory status
            found <- forM names $ \n -> (statusFrom <=< Aeson.decode) <$> LBS.readFile (status </> n)
            let
                created v = case v of
                    Object o | Just (String c) <- KeyMap.lookup "created" o -> c
                    _ -> ""
            respond (Wai.responseLBS Http.status200 json (Aeson.encode (take 50 (sortOn (Down . created) (catMaybes found)))))
        ("GET", ["api", "renders", jid, "video"])
            | Text.all (\c -> c `elem` ['a' .. 'f'] || c `elem` ['0' .. '9'] || c == '-') jid && not (Text.null jid) -> do
                -- An MP4, or (ProRes) a QuickTime movie.
                let
                    candidates = [(done </> Text.unpack jid <> ext, ext, ctype) | (ext, ctype) <- [(".mp4", "video/mp4"), (".mov", "video/quicktime")]]
                found <- filterM (\(path, _, _) -> doesFileExist path) candidates
                case found of
                    (path, ext, ctype) : _ ->
                        respond (Wai.responseFile Http.status200 [("Content-Type", ctype), ("Content-Disposition", "attachment; filename=\"anima2d-" <> Text.encodeUtf8 (Text.take 8 jid) <> BS8.pack ext <> "\"")] path Nothing)
                    [] -> respond (jsonError Http.status404 "no such video")
        _ -> respond (jsonError Http.status404 "not found")
    where
        json = [("Content-Type", "application/json")]
        jsonError s why = Wai.responseLBS s json (Aeson.encode (object ["error" .= why]))

{- | One frame drawn by the Mac as a PNG, for an agent to look at what it drew. The job
goes in the same queue (the agent takes stills before videos) and is waited for here;
nothing is listed among the renders.
-}
previewFrame :: FilePath -> IO Text -> Project -> Int -> Int -> IO (Either Text LBS.ByteString)
previewFrame dataDir newId project frame width = do
    let
        root = dataDir </> "renders"
        queue = root </> "queue"
        done = root </> "done"
    mapM_ (createDirectoryIfMissing True) [queue, root </> "working", root </> "status", done]
    jid <- newId
    now <- getCurrentTime
    let
        name = Text.unpack jid
        -- Named apart from video jobs, so the agent can take it in the middle of one.
        job = queue </> name <> ".still.json"
        png = done </> name <> ".png"
        failed = done </> name <> ".error"
    writeWhole job (object ["id" .= jid, "still" .= object ["frame" .= frame, "width" .= width], "project" .= unparse project, "created" .= iso8601Show now])
    -- Half a second at a time, for up to 40 seconds.
    let
        waitFor :: Int -> IO (Either Text LBS.ByteString)
        waitFor 0 = do
            queued <- doesFileExist job
            when queued (removeFile job)
            pure (Left "The Mac did not draw the preview in time: is it awake, with the render agent running? (A long video render ahead of it also waits.)")
        waitFor n = do
            drawn <- doesFileExist png
            broke <- doesFileExist failed
            if
                | drawn -> do
                    bytes <- BS.readFile png
                    removeFile png
                    pure (Right (LBS.fromStrict bytes))
                | broke -> do
                    why <- BS.readFile failed
                    removeFile failed
                    pure (Left ("The Mac could not draw it: " <> Text.decodeUtf8Lenient why))
                | otherwise -> threadDelay 500_000 >> waitFor (n - 1)
    waitFor 80

-- | The job and its first status, each written whole and renamed into place.
queueJob :: FilePath -> FilePath -> Text -> Project -> Value -> Text -> IO ()
queueJob queue status jid project preset created = do
    let
        name = Text.unpack jid <> ".json"
        presetName = case preset of
            Object o | Just (String n) <- KeyMap.lookup "name" o -> n
            _ -> ""
        projectName = case unparse project of
            Object o | Just (String n) <- KeyMap.lookup "name" o -> n
            _ -> unparse (projectId project)
    writeWhole (status </> name) (object ["id" .= jid, "state" .= ("queued" :: Text), "progress" .= (0 :: Int), "project" .= projectName, "preset" .= presetName, "created" .= created, "message" .= ("Waiting for the Mac" :: Text)])
    writeWhole (queue </> name) (object ["id" .= jid, "preset" .= preset, "project" .= unparse project, "created" .= created])

writeWhole :: FilePath -> Value -> IO ()
writeWhole path v = do
    LBS.writeFile (path <> ".part") (Aeson.encode v)
    renameFile (path <> ".part") path
