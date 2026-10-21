{- | The HTTP side: the projects API and the editor's static files.

Sign-in is not here. the host's single sign-on gate (a reverse proxy asking an OAuth2 proxy)
stands in front of every request, so anything that reaches this server has already
signed in.
-}
module Anima.Server
    ( Api
    , app
    )
where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Servant
    ( Capture
    , Get
    , Handler
    , JSON
    , PlainText
    , Put
    , Raw
    , ReqBody
    , Server
    , ServerError (..)
    , err400
    , err404
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

import Data.Text qualified as Text
import Network.Wai qualified as Wai

import Anima.Store (ProjectStore (..), StoreError (..))
import Anima.Types.Project (Project, ProjectId, ProjectSummary, projectId)

type Api =
    "healthz" :> Get '[PlainText] Text
        :<|> "api" :> "projects" :> Get '[JSON] [ProjectSummary]
        :<|> "api" :> "projects" :> Capture "id" ProjectId :> Get '[JSON] Project
        :<|> "api" :> "projects" :> Capture "id" ProjectId :> ReqBody '[JSON] Project :> Put '[JSON] ProjectSummary
        :<|> Raw

-- | The application, given where projects live and where the built editor is.
app :: ProjectStore -> FilePath -> Wai.Application
app store staticDir = serve (Proxy @Api) (server store staticDir)

server :: ProjectStore -> FilePath -> Server Api
server store staticDir =
    pure "ok"
        :<|> (liftIO store.list >>= orFail)
        :<|> (\pid -> liftIO (store.load pid) >>= orFail)
        :<|> putProject
        :<|> serveDirectoryWith editorFiles
    where
        editorFiles =
            (defaultWebAppSettings staticDir)
                { ssIndices = mapMaybe (toPiece . Text.pack) ["index.html"]
                , -- Revalidate everything (ETags), so a deploy reaches open editors on reload.
                  ssMaxAge = MaxAgeSeconds 0
                }
        putProject pid project
            | projectId project /= pid = throwError err400 {errBody = "expected the body's id to match the path"}
            | otherwise = liftIO (store.save project) >>= orFail

orFail :: Either StoreError a -> Handler a
orFail = \case
    Right a -> pure a
    Left NotFound -> throwError err404 {errBody = "no such project"}
    Left (Corrupt why) -> do
        -- The file name and the parser's expectation; never the file's contents.
        liftIO (hPutStrLn stderr ("corrupt project file: " <> Text.unpack why))
        throwError err500 {errBody = "a stored project could not be read"}
    Left (Unavailable what) -> do
        liftIO (hPutStrLn stderr ("storage unavailable: could not " <> Text.unpack what))
        throwError err503 {errBody = "storage is unavailable"}
