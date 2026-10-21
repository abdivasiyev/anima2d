module Main (main) where

import Control.Concurrent (forkIO)
import Data.Bits ((.&.), (.|.))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Numeric (showHex)
import Options.Applicative (command, execParser, fullDesc, header, helper, hsubparser, info, progDesc, (<**>))
import System.Environment (lookupEnv)
import System.IO (BufferMode (..), IOMode (..), hSetBuffering, stderr, stdout, withBinaryFile)

import Data.ByteString qualified as BS
import Data.Text qualified as Text

import Anima.Config (Config (..), configParser, portNumber)
import Anima.Mcp (mcpApp)
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
            _ <- forkIO (runSettings (listen p) (mcpApp store newUuid))
            pure ()
    putStrLn ("anima2d listening on 127.0.0.1:" <> show (portNumber cfg.port))
    runSettings (listen cfg.port) (app store cfg.staticDir)

-- | A random (version 4) UUID, for projects an agent creates.
newUuid :: IO Text.Text
newUuid = do
    bytes <- BS.unpack <$> withBinaryFile "/dev/urandom" ReadMode (`BS.hGet` 16)
    let
        -- Version 4 in byte 6, the RFC 4122 variant in byte 8.
        v4 = [if j == 6 then (b .&. 0x0f) .|. 0x40 else if j == 8 then (b .&. 0x3f) .|. 0x80 else b | (j, b) <- zip [0 :: Int ..] bytes]
        hex = concatMap (\b -> let h = showHex b "" in if length h == 1 then '0' : h else h) v4
        (a, r1) = splitAt 8 hex
        (b', r2) = splitAt 4 r1
        (c, r3) = splitAt 4 r2
        (d, e) = splitAt 4 r3
    pure (Text.pack (intercalate "-" [a, b', c, d, e]))
