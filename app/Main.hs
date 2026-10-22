module Main (main) where

import Control.Concurrent (forkIO)
import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Options.Applicative (command, execParser, fullDesc, header, helper, hsubparser, info, progDesc, (<**>))
import System.Environment (lookupEnv)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

import Network.Wai qualified as Wai

import Anima.Assets (assetsApp)
import Anima.Config (Config (..), configParser, portNumber)
import Anima.Ids (newUuid)
import Anima.Mcp (mcpApp)
import Anima.Renders (previewFrame, rendersApp)
import Anima.Server (app)
import Anima.Store (fileStore)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    -- The nix wrapper points this at the editor built beside the binary.
    defaultStatic <- fromMaybe "web/dist" <$> lookupEnv "ANIMA2D_STATIC"
    let
        serveCmd = command "serve" (info (configParser defaultStatic) (progDesc "Serve the editor, the projects API and (optionally) MCP"))
    cfg <- execParser (info (hsubparser serveCmd <**> helper) (fullDesc <> header "anima2d: a 2D animation studio"))
    store <- fileStore cfg.dataDir
    let
        listen p = setHost "127.0.0.1" (setPort (portNumber p) defaultSettings)
    case cfg.mcpPort of
        Nothing -> pure ()
        Just p -> do
            putStrLn ("anima2d MCP on 127.0.0.1:" <> show (portNumber p))
            _ <- forkIO (runSettings (listen p) (mcpApp store newUuid (previewFrame cfg.dataDir newUuid)))
            pure ()
    putStrLn ("anima2d listening on 127.0.0.1:" <> show (portNumber cfg.port))
    -- Uploaded files are streamed outside Servant, which would read a whole body into memory.
    let
        web = app store cfg.staticDir
        assets = assetsApp cfg.dataDir newUuid
        renders = rendersApp cfg.dataDir store newUuid
    runSettings (listen cfg.port) $ \req respond ->
        case Wai.pathInfo req of
            ("api" : "assets" : _) -> assets req respond
            ("api" : "renders" : _) -> renders req respond
            _ -> web req respond
