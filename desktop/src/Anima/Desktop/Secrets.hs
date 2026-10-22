{- | The assistant's API key, kept by the system rather than in a settings file: the login
keychain on macOS, the Secret Service (secret-tool) on Linux, and only when neither is
there, a file only this user can read. The key never goes on a command line; it is
written to the helper's standard input.
-}
module Anima.Desktop.Secrets
    ( ApiKey
    , parseApiKey
    , keyText
    , loadKey
    , saveKey
    )
where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Char (isAscii, isPrint, isSpace)
import Data.Text (Text)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, findExecutable, getXdgDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Info (os)
import System.Posix.Files (ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)
import System.Process (readProcessWithExitCode)

import Data.Text qualified as Text
import Data.Text.IO qualified as Text

-- | A key as the providers issue them: printable ASCII, no spaces or quotes.
newtype ApiKey = ApiKey Text

parseApiKey :: Text -> Either Text ApiKey
parseApiKey raw =
    let
        k = Text.strip raw
    in
        if not (Text.null k) && Text.length k <= 400 && Text.all (\c -> isAscii c && isPrint c && not (isSpace c) && c `notElem` ("'\"\\" :: String)) k
            then Right (ApiKey k)
            else Left "expected a key of letters, digits and dashes, as the provider gave it"

keyText :: ApiKey -> Text
keyText (ApiKey k) = k

service :: String
service = "anima2d-desktop"

-- | The key saved for `account` (a provider), if any.
loadKey :: Text -> IO (Maybe ApiKey)
loadKey account = do
    found <-
        if os == "darwin"
            then helper "security" ["find-generic-password", "-s", service, "-a", Text.unpack account, "-w"] ""
            else do
                st <- findExecutable "secret-tool"
                case st of
                    Just _ -> helper "secret-tool" ["lookup", "service", service, "account", Text.unpack account] ""
                    Nothing -> fileKey account >>= \f -> doesFileExist f >>= \e -> if e then Just . Text.unpack <$> Text.readFile f else pure Nothing
    pure (found >>= either (const Nothing) Just . parseApiKey . Text.pack)

-- | Keeps `key` for `account`, replacing an older one.
saveKey :: Text -> ApiKey -> IO (Either Text ())
saveKey account (ApiKey k) =
    if os == "darwin"
        then done <$> helper "security" ["-i"] ("add-generic-password -U -s " <> service <> " -a " <> Text.unpack account <> " -w '" <> Text.unpack k <> "'\n")
        else do
            st <- findExecutable "secret-tool"
            case st of
                Just _ -> done <$> helper "secret-tool" ["store", "--label", "anima2D assistant key", "service", service, "account", Text.unpack account] (Text.unpack k)
                Nothing -> do
                    f <- fileKey account
                    r <- try $ do
                        exists <- doesFileExist f
                        when exists (removeFile f)
                        Text.writeFile f ""
                        setFileMode f (unionFileModes ownerReadMode ownerWriteMode)
                        Text.writeFile f k
                    pure (either (\(_ :: SomeException) -> Left "the key could not be kept") Right r)
    where
        done = maybe (Left "the system keychain did not take the key") (const (Right ()))

fileKey :: Text -> IO FilePath
fileKey account = do
    dir <- getXdgDirectory XdgConfig "anima2d-desktop"
    createDirectoryIfMissing True dir
    pure (dir </> ("key-" <> Text.unpack account))

-- | A helper's output when it succeeds (trailing newline dropped).
helper :: FilePath -> [String] -> String -> IO (Maybe String)
helper exe args stdin = do
    r <- try (readProcessWithExitCode exe args stdin)
    pure $ case r of
        Right (ExitSuccess, out, _) -> Just (reverse (dropWhile (== '\n') (reverse out)))
        Right _ -> Nothing
        Left (_ :: SomeException) -> Nothing
