{- | The HTTP side: the projects API and the editor's static files.

Sign-in is not here. A single sign-on gate in front (a reverse proxy asking an OAuth2
proxy) stands before every request, so anything that reaches this server has already
signed in.

Saves are revisioned. A project is loaded with its revision (@X-Revision@); a save names
the revision it started from (@X-Base-Revision@) and is refused with 409 if another save
landed first, so two devices, or the editor and an agent, never overwrite each other.
-}
module Anima.Server
    ( Api
    , app
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Servant
    ( Capture
    , DeleteNoContent
    , Get
    , Handler
    , Header
    , Headers
    , JSON
    , NoContent (..)
    , PlainText
    , Put
    , QueryParam
    , Raw
    , ReqBody
    , Server
    , ServerError (..)
    , addHeader
    , err400
    , err404
    , err409
    , err500
    , err503
    , serve
    , serveDirectoryWith
    , throwError
    , (:<|>) (..)
    , (:>)
    )
import System.IO (hPutStrLn, stderr)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (MaxAge (..), StaticSettings (..), toPiece)

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Network.Wai qualified as Wai

import Anima.Store (ProjectStore (..), StoreError (..))
import Anima.Types.Parse (Parse (unparse))
import Anima.Types.Project (Project, ProjectId, ProjectSummary, Revision, projectId, summaryRevision)

type Api =
    "healthz" :> Get '[PlainText] Text
        :<|> "api" :> "projects" :> Get '[JSON] [ProjectSummary]
        :<|> "api" :> "projects" :> Capture "id" ProjectId :> Get '[JSON] (Headers '[Header "X-Revision" Revision] Project)
        :<|> "api" :> "projects" :> Capture "id" ProjectId :> "summary" :> Get '[JSON] ProjectSummary
        :<|> "api"
            :> "projects"
            :> Capture "id" ProjectId
            :> Header "X-Base-Revision" Revision
            :> ReqBody '[JSON] Project
            :> Put '[JSON] ProjectSummary
        -- Deleting moves the project's files to trash/ on the server.
        :<|> "api" :> "projects" :> Capture "id" ProjectId :> DeleteNoContent
        -- Long-polling: answers when anything is saved after save number @after@ (or after
        -- a while with nothing new), with the latest number; editors then look again.
        :<|> "api" :> "changes" :> QueryParam "after" Int :> Get '[JSON] Aeson.Value
        :<|> Raw

-- | The application, given where projects live and where the built editor is.
app :: ProjectStore -> FilePath -> Wai.Application
app store staticDir = serve (Proxy @Api) (server store staticDir)

server :: ProjectStore -> FilePath -> Server Api
server store staticDir =
    pure "ok"
        :<|> (liftIO store.list >>= orFail)
        :<|> getProject
        :<|> (\pid -> snd <$> (liftIO (store.load pid) >>= orFail))
        :<|> putProject
        :<|> (\pid -> NoContent <$ (liftIO (store.remove pid) >>= orFail))
        :<|> (\after -> (\n -> Aeson.object ["seq" Aeson..= n]) <$> liftIO (store.changes (fromMaybe (-1) after)))
        :<|> serveDirectoryWith editorFiles
    where
        editorFiles =
            (defaultWebAppSettings staticDir)
                { ssIndices = mapMaybe (toPiece . Text.pack) ["index.html"]
                , -- Revalidate everything (ETags), so a deploy reaches open editors on reload.
                  ssMaxAge = MaxAgeSeconds 0
                }
        getProject pid = do
            (project, summary) <- liftIO (store.load pid) >>= orFail
            pure (addHeader (summaryRevision summary) project)
        putProject pid base project
            | projectId project /= pid = throwError err400 {errBody = "expected the body's id to match the path"}
            | otherwise = liftIO (store.save base project) >>= orFail

orFail :: Either StoreError a -> Handler a
orFail = \case
    Right a -> pure a
    Left NotFound -> throwError err404 {errBody = "no such project"}
    Left (Conflict now) ->
        -- The current revision, so the editor can tell the user what happened.
        throwError err409 {errBody = Aeson.encode (Aeson.object ["revision" Aeson..= unparse now]), errHeaders = [("Content-Type", "application/json")]}
    Left (Corrupt why) -> do
        -- The file name and the parser's expectation; never the file's contents.
        liftIO (hPutStrLn stderr ("corrupt project file: " <> Text.unpack why))
        throwError err500 {errBody = "a stored project could not be read"}
    Left (Unavailable what) -> do
        liftIO (hPutStrLn stderr ("storage unavailable: could not " <> Text.unpack what))
        throwError err503 {errBody = "storage is unavailable"}
