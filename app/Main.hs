module Main (main) where

import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Options.Applicative (command, execParser, fullDesc, header, helper, hsubparser, info, progDesc, (<**>))
import System.Environment (lookupEnv)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

import Anima.Config (Config (..), configParser, portNumber)
import Anima.Server (app)
import Anima.Store (fileStore)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    -- The nix wrapper points this at the editor built beside the binary.
    defaultStatic <- fromMaybe "web/dist" <$> lookupEnv "ANIMA2D_STATIC"
    let
        serveCmd = command "serve" (info (configParser defaultStatic) (progDesc "Serve the editor and the projects API"))
    cfg <- execParser (info (hsubparser serveCmd <**> helper) (fullDesc <> header "anima2d: a 2D animation studio"))
    store <- fileStore cfg.dataDir
    putStrLn ("anima2d listening on 127.0.0.1:" <> show (portNumber cfg.port))
    runSettings (setHost "127.0.0.1" (setPort (portNumber cfg.port) defaultSettings)) (app store cfg.staticDir)
