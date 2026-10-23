{- | Secrets (the assistant's API key, a sign-in's refresh token), kept by the system rather
than in a settings file: the login keychain on macOS, the Secret Service (secret-tool) on
Linux, and only when neither is there, a file only this user can read. A secret never
goes on a command line; it is written to the helper's standard input.
-}
module Anima.Desktop.Secrets
    ( ApiKey
    , parseApiKey
    , keyText
    , loadKey
    , saveKey
    , saveSecret
    , loadSecret
    , forgetSecret
    )
where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Char (isAlphaNum, isAscii, isPrint, isSpace)
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
loadKey account = (>>= either (const Nothing) Just . parseApiKey) <$> fetch account

-- | Keeps `key` for `account`, replacing an older one.
saveKey :: Text -> ApiKey -> IO (Either Text ())
saveKey account (ApiKey k) = store account k

{- | Any other secret (a sign-in's refresh token): printable, without spaces or quotes, as
tokens are; up to 16 000 characters.
-}
saveSecret :: Text -> Text -> IO (Either Text ())
saveSecret account v
    | Text.null v || Text.length v > 16000 || not (Text.all (\c -> isAscii c && isPrint c && not (isSpace c) && c `notElem` ("'\"\\" :: String)) v) = pure (Left "expected a token of printable characters")
    | otherwise = store account v

loadSecret :: Text -> IO (Maybe Text)
loadSecret account = fmap Text.strip <$> fetch account

forgetSecret :: Text -> IO (Either Text ())
forgetSecret account
    | os == "darwin" = Right () <$ helper "security" ["delete-generic-password", "-s", service, "-a", Text.unpack account] ""
    | otherwise = do
        st <- findExecutable "secret-tool"
        case st of
            Just _ -> Right () <$ helper "secret-tool" ["clear", "service", service, "account", Text.unpack account] ""
            Nothing -> do
                f <- fileKey account
                exists <- doesFileExist f
                r <- try (when exists (removeFile f))
                pure (either (\(_ :: SomeException) -> Left "the secret could not be forgotten") Right r)

fetch :: Text -> IO (Maybe Text)
fetch account =
    fmap Text.pack <$> do
        if os == "darwin"
            then helper "security" ["find-generic-password", "-s", service, "-a", Text.unpack account, "-w"] ""
            else do
                st <- findExecutable "secret-tool"
                case st of
                    Just _ -> helper "secret-tool" ["lookup", "service", service, "account", Text.unpack account] ""
                    Nothing -> fileKey account >>= \f -> doesFileExist f >>= \e -> if e then Just . Text.unpack <$> Text.readFile f else pure Nothing

-- | `v` kept for `account`; neither may hold a quote (the callers' parsers see to it).
store :: Text -> Text -> IO (Either Text ())
store account v
    | Text.any (`elem` ("'\n" :: String)) account = pure (Left "expected an account name without quotes")
    | os == "darwin" = done <$> helper "security" ["-i"] ("add-generic-password -U -s " <> service <> " -a '" <> Text.unpack account <> "' -w '" <> Text.unpack v <> "'\n")
    | otherwise = do
        st <- findExecutable "secret-tool"
        case st of
            Just _ -> done <$> helper "secret-tool" ["store", "--label", "anima2D: " <> Text.unpack account, "service", service, "account", Text.unpack account] (Text.unpack v)
            Nothing -> do
                f <- fileKey account
                r <- try $ do
                    exists <- doesFileExist f
                    when exists (removeFile f)
                    Text.writeFile f ""
                    setFileMode f (unionFileModes ownerReadMode ownerWriteMode)
                    Text.writeFile f v
                pure (either (\(_ :: SomeException) -> Left "the secret could not be kept") Right r)
    where
        done = maybe (Left "the system keychain did not take it") (const (Right ()))

fileKey :: Text -> IO FilePath
fileKey account = do
    dir <- getXdgDirectory XdgConfig "anima2d-desktop"
    createDirectoryIfMissing True dir
    pure (dir </> ("key-" <> map (\c -> if isAlphaNum c then c else '_') (Text.unpack account)))

-- | A helper's output when it succeeds (trailing newline dropped).
helper :: FilePath -> [String] -> String -> IO (Maybe String)
helper exe args stdin = do
    r <- try (readProcessWithExitCode exe args stdin)
    pure $ case r of
        Right (ExitSuccess, out, _) -> Just (reverse (dropWhile (== '\n') (reverse out)))
        Right _ -> Nothing
        Left (_ :: SomeException) -> Nothing
