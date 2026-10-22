{- | Which projects the window opens: a folder on this computer, or a self-hosted server
(its address, and any headers its sign-in gate asks for). Kept in the user's config
folder and parsed back like anything else read from outside; a file that does not parse
is as good as none, and the window asks again.
-}
module Anima.Desktop.Config
    ( Connection (..)
    , parseConnection
    , loadConnection
    , saveConnection
    , defaultFolder
    , cacheFolder
    )
where

import Control.Exception (SomeException, try)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text

data Connection
    = -- | Projects in this folder, through the server's own store in the app.
      Local FilePath
    | -- | A server's address, and headers to send with every request.
      Remote Text [(Text, Text)]
    deriving stock (Eq, Show)

parseConnection :: Aeson.Value -> Either Text Connection
parseConnection = \case
    Aeson.Object o -> case KeyMap.lookup "kind" o of
        Just (Aeson.String "local") -> case KeyMap.lookup "folder" o of
            Just (Aeson.String f) | not (Text.null (Text.strip f)) -> Right (Local (Text.unpack f))
            _ -> Left "folder: expected a folder"
        Just (Aeson.String "remote") -> do
            url <- case KeyMap.lookup "url" o of
                Just (Aeson.String u) -> parseUrl u
                _ -> Left "url: expected the server's address"
            hs <- case KeyMap.lookup "headers" o of
                Nothing -> Right []
                Just (Aeson.Object h) -> traverse header (KeyMap.toList h)
                Just _ -> Left "headers: expected an object of names and values"
            Right (Remote url hs)
        _ -> Left "kind: expected \"local\" or \"remote\""
    _ -> Left "expected an object"
    where
        header (k, v) = case v of
            Aeson.String s | validName (Key.toText k) && Text.all (\c -> c >= ' ' && c /= '\DEL') s -> Right (Key.toText k, s)
            _ -> Left ("headers: " <> Key.toText k <> ": expected a header name and a text value")
        validName n = not (Text.null n) && Text.all (\c -> isAlphaNum c || c `elem` ("-_" :: String)) n

-- | An http(s) address, without a trailing slash.
parseUrl :: Text -> Either Text Text
parseUrl raw =
    let
        u = Text.dropWhileEnd (== '/') (Text.strip raw)
    in
        if ("https://" `Text.isPrefixOf` u || "http://" `Text.isPrefixOf` u) && Text.length u > 8 && not (Text.any (== ' ') u)
            then Right u
            else Left "url: expected an address starting with https:// or http://"

configFile :: IO FilePath
configFile = do
    dir <- getXdgDirectory XdgConfig "anima2d-desktop"
    createDirectoryIfMissing True dir
    pure (dir </> "connection.json")

loadConnection :: IO (Maybe Connection)
loadConnection = do
    path <- configFile
    raw <- try (LBS.readFile path)
    pure $ case raw of
        Left (_ :: SomeException) -> Nothing
        Right bytes -> either (const Nothing) Just (maybe (Left "") parseConnection (Aeson.decode bytes))

saveConnection :: Connection -> IO ()
saveConnection c = do
    path <- configFile
    LBS.writeFile path . Aeson.encode $ case c of
        Local f -> Aeson.object ["kind" Aeson..= ("local" :: Text), "folder" Aeson..= f]
        Remote u hs -> Aeson.object ["kind" Aeson..= ("remote" :: Text), "url" Aeson..= u, "headers" Aeson..= Aeson.object [Key.fromText k Aeson..= v | (k, v) <- hs]]

-- | Where projects go on this computer unless another folder is chosen.
defaultFolder :: IO FilePath
defaultFolder = getXdgDirectory XdgData "anima2d"

-- | Where a server's pictures are kept once fetched.
cacheFolder :: IO FilePath
cacheFolder = do
    dir <- getXdgDirectory XdgCache "anima2d-desktop"
    createDirectoryIfMissing True dir
    pure dir
