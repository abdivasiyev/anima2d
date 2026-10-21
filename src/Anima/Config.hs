{-# LANGUAGE TypeFamilies #-}

{- | What @anima2d serve@ is started with, parsed once at startup.

Startup is the one place that throws: a bad flag stops the process with a message naming
the flag, before anything listens.
-}
module Anima.Config
    ( Config (..)
    , Port
    , portNumber
    , configParser
    )
where

import Options.Applicative (Parser, eitherReader, help, long, metavar, option, optional, strOption, value)

import Data.Text qualified as Text

import Anima.Types.Parse (Parse (..))

-- | A TCP port to listen on, 1024 to 65535 (the service never runs as root).
newtype Port = Port Int
    deriving stock (Eq, Show)

instance Parse Port where
    type Raw Port = Int
    parse raw
        | raw >= 1024 && raw <= 65535 = Right (Port raw)
        | otherwise = Left "expected a port from 1024 to 65535"
    unparse (Port raw) = raw

portNumber :: Port -> Int
portNumber (Port raw) = raw

data Config = Config
    { port :: Port
    , mcpPort :: Maybe Port
    , dataDir :: FilePath
    , staticDir :: FilePath
    }

configParser :: FilePath -> Parser Config
configParser defaultStatic =
    Config
        <$> option (eitherReader readPort) (long "port" <> metavar "PORT" <> value (Port 9215) <> help "Loopback port to listen on")
        <*> optional (option (eitherReader readPort) (long "mcp-port" <> metavar "PORT" <> help "Loopback port for the MCP server (off if not given)"))
        <*> strOption (long "data" <> metavar "DIR" <> help "Directory projects are kept in")
        <*> strOption (long "static" <> metavar "DIR" <> value defaultStatic <> help "The built editor")
    where
        readPort :: String -> Either String Port
        readPort s = case reads s of
            [(n, "")] -> either (Left . ("--port: " <>) . Text.unpack) Right (parse n)
            _ -> Left "--port: expected a number"
