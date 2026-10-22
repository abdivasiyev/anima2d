{-# LANGUAGE MultiWayIf #-}

{- | Uploaded files: reference images and videos to trace over, and audio tracks.

Uploads stream to disk (a reference video is far larger than the service's memory cap)
and are refused past 'maxBytes' or when they are not an image, a video or audio. Files
never change once stored, so they are served with a long cache lifetime; Warp answers
range requests, which video seeking needs.
-}
module Anima.Assets (assetsApp) where

import Control.Exception (bracketOnError)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath ((</>))
import System.IO (hClose, openBinaryTempFile)

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

import Anima.Types.Parse (Parse (..))
import Anima.Types.Project (AssetId)

-- | The largest upload: a few minutes of reference video.
maxBytes :: Int
maxBytes = 512 * 1024 * 1024

-- | Pictures, video, sound, and font files (the editor names a font's type by its extension).
allowed :: BS.ByteString -> Bool
allowed t = any (`BS.isPrefixOf` t) ["image/", "video/", "audio/", "font/"]

{- | @POST /api/assets@ (the body is the file, its type in Content-Type) answers @{"id"}@;
@GET /api/assets/<id>@ serves it back.
-}
assetsApp :: FilePath -> IO Text -> Wai.Application
assetsApp dataDir newId req respond = do
    let
        dir = dataDir </> "assets"
    createDirectoryIfMissing True dir
    case (Wai.requestMethod req, Wai.pathInfo req) of
        ("POST", ["api", "assets"]) -> do
            let
                ctype = maybe "" (BS8.takeWhile (/= ';')) (lookup "Content-Type" (Wai.requestHeaders req))
            if not (allowed ctype)
                then respond (plain Http.status415 "expected an image, a video, audio or a font")
                else do
                    raw <- newId
                    case parse @AssetId raw of
                        Left _ -> respond (plain Http.status500 "could not name the file")
                        Right aid -> do
                            stored <- streamTo dir (Text.unpack (unparse aid))
                            case stored of
                                Left why -> respond (plain Http.status413 (LBS.fromStrict why))
                                Right () -> do
                                    BS.writeFile (dir </> Text.unpack (unparse aid) <> ".type") ctype
                                    respond (Wai.responseLBS Http.status201 [("Content-Type", "application/json")] (Aeson.encode (Aeson.object ["id" Aeson..= unparse aid])))
        ("GET", ["api", "assets", raw]) -> case parse @AssetId raw of
            Left _ -> respond (plain Http.status404 "no such file")
            Right aid -> do
                let
                    path = dir </> Text.unpack (unparse aid)
                exists <- doesFileExist path
                if not exists
                    then respond (plain Http.status404 "no such file")
                    else do
                        ctype <- BS.readFile (path <> ".type")
                        respond (Wai.responseFile Http.status200 [("Content-Type", ctype), ("Cache-Control", "private, max-age=31536000, immutable")] path Nothing)
        _ -> respond (plain Http.status404 "not found")
    where
        plain s = Wai.responseLBS s [("Content-Type", "text/plain")]
        -- The body, chunk by chunk, into a temporary file renamed into place when complete.
        streamTo dir name =
            bracketOnError (openBinaryTempFile dir (name <> ".part")) (\(tmp, h) -> hClose h >> removeFile tmp) $ \(tmp, h) -> do
                let
                    loop n = do
                        chunk <- Wai.getRequestBodyChunk req
                        if
                            | BS.null chunk -> pure (Right ())
                            | n + BS.length chunk > maxBytes -> pure (Left ("expected a file of at most " <> show' (maxBytes `div` (1024 * 1024)) <> " MB"))
                            | otherwise -> BS.hPut h chunk >> loop (n + BS.length chunk)
                result <- loop 0
                hClose h
                case result of
                    Right () -> renameFile tmp (dir </> name) >> pure (Right ())
                    Left why -> removeFile tmp >> pure (Left why)
        show' = Text.encodeUtf8 . Text.pack . show
