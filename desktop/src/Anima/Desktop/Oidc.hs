{- | Signing in to a server behind Keycloak (or any OpenID Connect provider), as a native
app should (RFC 8252): the authorization code flow with PKCE (RFC 7636) in the system's
browser, its answer caught by a listener on the loopback address, so the app never sees
the password. The refresh token is kept in the system keychain; the access token is kept
in memory and renewed before it runs out.

Everything read from the provider (its discovery document, its tokens) is parsed before
it is used.
-}
module Anima.Desktop.Oidc
    ( Oidc (..)
    , parseOidc
    , oidcValue
    , redirectPort
    , redirectUri
    , Provider (..)
    , parseDiscovery
    , discover
    , Tokens (..)
    , parseTokens
    , challengeOf
    , authorizeUrl
    , signIn
    , refresh
    , displayName
    , Auth (..)
    , newAuth
    , keychainAccount
    )
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.Random (getRandomBytes)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Timeout (timeout)

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types qualified as HttpTypes
import Network.HTTP.Types.URI qualified as Uri
import Network.Socket qualified as Net
import Network.Socket.ByteString qualified as NetB

import Anima.Desktop.Secrets (forgetSecret, loadSecret, saveSecret)

{- | Where to sign in: the provider's issuer (a Keycloak realm's address) and this app's
client there (a public one, with PKCE).
-}
data Oidc = Oidc {issuer :: Text, clientId :: Text}
    deriving stock (Eq, Show)

parseOidc :: Aeson.Value -> Either Text Oidc
parseOidc = \case
    Aeson.Object o -> do
        iss <- case KeyMap.lookup "issuer" o of
            Just (Aeson.String t) -> parseAddress "issuer" t
            _ -> Left "issuer: expected the realm's address, like https://auth.example.com/realms/anima"
        cid <- case KeyMap.lookup "clientId" o of
            Just (Aeson.String t)
                | let
                    c = Text.strip t
                , not (Text.null c) && Text.length c <= 200 && Text.all (\ch -> ch > ' ' && ch `notElem` ("'\"\\" :: String)) c ->
                    Right c
            _ -> Left "clientId: expected the client's id in the realm"
        Right (Oidc iss cid)
    _ -> Left "expected an object"

oidcValue :: Oidc -> Aeson.Value
oidcValue o = Aeson.object ["issuer" Aeson..= o.issuer, "clientId" Aeson..= o.clientId]

{- | An address the app will send a person to: https, or http only on this computer (for
a provider in development).
-}
parseAddress :: Text -> Text -> Either Text Text
parseAddress what raw =
    let
        u = Text.dropWhileEnd (== '/') (Text.strip raw)
        local = any (`Text.isPrefixOf` u) ["http://127.0.0.1", "http://localhost", "http://[::1]"]
    in
        if ("https://" `Text.isPrefixOf` u || local) && Text.length u > 8 && not (Text.any (\c -> c <= ' ' || c `elem` ("'\"\\" :: String)) u)
            then Right u
            else Left (what <> ": expected an https:// address")

-- | The loopback port the provider sends the browser back to; register it with the client.
redirectPort :: Int
redirectPort = 51973

redirectUri :: Int -> Text
redirectUri port = "http://127.0.0.1:" <> Text.pack (show port) <> "/callback"

-- | The provider's endpoints, from its discovery document.
data Provider = Provider {authorizeEndpoint :: Text, tokenEndpoint :: Text}
    deriving stock (Eq, Show)

parseDiscovery :: Text -> Aeson.Value -> Either Text Provider
parseDiscovery iss = \case
    Aeson.Object o -> do
        claimed <- text "issuer" o
        if Text.dropWhileEnd (== '/') claimed /= iss
            then Left ("the provider calls itself " <> claimed <> ", not " <> iss)
            else Provider <$> (text "authorization_endpoint" o >>= parseAddress "authorization_endpoint") <*> (text "token_endpoint" o >>= parseAddress "token_endpoint")
    _ -> Left "the discovery document was not an object"
    where
        text k o = case KeyMap.lookup k o of
            Just (Aeson.String t) -> Right t
            _ -> Left (Text.pack (show k) <> ": missing from the discovery document")

discover :: Http.Manager -> Oidc -> IO (Either Text Provider)
discover manager o = do
    r <- get manager (o.issuer <> "/.well-known/openid-configuration")
    pure (r >>= decodeJson >>= parseDiscovery o.issuer)

-- | What the token endpoint gives: the access token, when it runs out, and the rest.
data Tokens = Tokens {access :: Text, refreshToken :: Maybe Text, expiresAt :: Double, idToken :: Maybe Text}
    deriving stock (Eq, Show)

-- | A token response, `now` being when it came (seconds since the epoch).
parseTokens :: Double -> Aeson.Value -> Either Text Tokens
parseTokens now = \case
    Aeson.Object o -> case KeyMap.lookup "error" o of
        Just (Aeson.String e) -> Left (e <> maybe "" (": " <>) (str "error_description" o))
        _ -> do
            tok <- maybe (Left "the provider sent no access token") Right (str "access_token" o)
            case str "token_type" o of
                Just t | Text.toLower t /= "bearer" -> Left ("expected a bearer token, got " <> t)
                _ -> pure ()
            let
                life = case KeyMap.lookup "expires_in" o of
                    Just (Aeson.Number n) -> realToFrac n
                    _ -> 300
            Right Tokens {access = tok, refreshToken = str "refresh_token" o, expiresAt = now + life, idToken = str "id_token" o}
    _ -> Left "the token response was not an object"
    where
        str k o = case KeyMap.lookup k o of
            Just (Aeson.String t) | not (Text.null t) -> Just t
            _ -> Nothing

-- | The S256 challenge of a PKCE verifier.
challengeOf :: ByteString -> Text
challengeOf verifier = base64url (BA.convert (hash verifier :: Digest SHA256))

base64url :: ByteString -> Text
base64url = Text.dropWhileEnd (== '=') . Text.decodeUtf8 . B64.encode

randomToken :: Int -> IO Text
randomToken n = base64url <$> getRandomBytes n

authorizeUrl :: Provider -> Oidc -> Int -> Text -> Text -> Text
authorizeUrl p o port state challenge =
    p.authorizeEndpoint
        <> (if "?" `Text.isInfixOf` p.authorizeEndpoint then "&" else "?")
        <> Text.decodeUtf8
            ( Uri.renderSimpleQuery
                False
                [ ("response_type", "code")
                , ("client_id", Text.encodeUtf8 o.clientId)
                , ("redirect_uri", Text.encodeUtf8 (redirectUri port))
                , ("scope", "openid profile email offline_access")
                , ("state", Text.encodeUtf8 state)
                , ("code_challenge", Text.encodeUtf8 challenge)
                , ("code_challenge_method", "S256")
                ]
            )

{- | The sign-in: `openBrowser` is given the provider's page; its answer comes back to the
listener on `port`. Five minutes at most.
-}
signIn :: Http.Manager -> Oidc -> Int -> (Text -> IO ()) -> IO (Either Text Tokens)
signIn manager o port openBrowser = do
    found <- discover manager o
    case found of
        Left why -> pure (Left why)
        Right p -> do
            verifier <- randomToken 48
            state <- randomToken 24
            caught <- listenOnce port
            case caught of
                Left why -> pure (Left why)
                Right (waitForAnswer, stopListening) -> do
                    openBrowser (authorizeUrl p o port state (challengeOf (Text.encodeUtf8 verifier)))
                    answer <- timeout (5 * 60 * 1000000) waitForAnswer
                    stopListening
                    case answer of
                        Nothing -> pure (Left "the sign-in took too long; try again")
                        Just q
                            | lookup "state" q /= Just state -> pure (Left "the sign-in's answer was not for this request")
                            | Just e <- lookup "error" q -> pure (Left (e <> maybe "" (": " <>) (lookup "error_description" q)))
                            | Just code <- lookup "code" q ->
                                tokenRequest manager p.tokenEndpoint [("grant_type", "authorization_code"), ("code", code), ("redirect_uri", redirectUri port), ("client_id", o.clientId), ("code_verifier", verifier)]
                            | otherwise -> pure (Left "the provider sent no code")

-- | New tokens for a refresh token.
refresh :: Http.Manager -> Oidc -> Text -> IO (Either Text Tokens)
refresh manager o tok = do
    found <- discover manager o
    case found of
        Left why -> pure (Left why)
        Right p -> tokenRequest manager p.tokenEndpoint [("grant_type", "refresh_token"), ("refresh_token", tok), ("client_id", o.clientId)]

tokenRequest :: Http.Manager -> Text -> [(Text, Text)] -> IO (Either Text Tokens)
tokenRequest manager endpoint form = do
    now <- realToFrac <$> getPOSIXTime
    r <- try $ do
        req0 <- Http.parseRequest (Text.unpack endpoint)
        let
            req = Http.urlEncodedBody [(Text.encodeUtf8 k, Text.encodeUtf8 v) | (k, v) <- form] req0 {Http.responseTimeout = Http.responseTimeoutMicro 30000000}
        Http.httpLbs req manager
    pure $ case r of
        Left (e :: SomeException) -> Left ("the provider is out of reach (" <> Text.take 120 (Text.pack (show e)) <> ")")
        Right res -> decodeJson (LBS.toStrict (Http.responseBody res)) >>= parseTokens now

get :: Http.Manager -> Text -> IO (Either Text ByteString)
get manager url = do
    r <- try $ do
        req <- Http.parseRequest (Text.unpack url)
        Http.httpLbs req {Http.responseTimeout = Http.responseTimeoutMicro 20000000} manager
    pure $ case r of
        Left (e :: SomeException) -> Left ("the provider is out of reach (" <> Text.take 120 (Text.pack (show e)) <> ")")
        Right res
            | HttpTypes.statusCode (Http.responseStatus res) /= 200 -> Left ("the provider answered " <> Text.pack (show (HttpTypes.statusCode (Http.responseStatus res))))
            | otherwise -> Right (LBS.toStrict (Http.responseBody res))

decodeJson :: ByteString -> Either Text Aeson.Value
decodeJson = either (Left . Text.pack) Right . Aeson.eitherDecodeStrict

{- | Listens on the loopback `port` for the browser's one request; gives a wait for its query
and a way to stop.
-}
listenOnce :: Int -> IO (Either Text (IO [(Text, Text)], IO ()))
listenOnce port = do
    opened <- try $ do
        sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
        Net.setSocketOption sock Net.ReuseAddr 1
        Net.bind sock (Net.SockAddrInet (fromIntegral port) (Net.tupleToHostAddress (127, 0, 0, 1)))
        Net.listen sock 4
        pure sock
    case opened of
        Left (e :: SomeException) -> pure (Left ("port " <> Text.pack (show port) <> " is taken (" <> Text.take 80 (Text.pack (show e)) <> ")"))
        Right sock -> do
            box <- newEmptyMVar
            let
                serve = do
                    (conn, _) <- Net.accept sock
                    request <- NetB.recv conn 8192
                    let
                        target = case BS8.words (BS8.takeWhile (/= '\r') request) of
                            _ : t : _ -> t
                            _ -> ""
                        (path, query) = BS8.break (== '?') target
                    if path /= "/callback"
                        then do
                            NetB.sendAll conn "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
                            Net.close conn
                            serve
                        else do
                            let
                                pairs = [(Text.decodeUtf8Lenient k, Text.decodeUtf8Lenient (fromMaybe "" v)) | (k, v) <- HttpTypes.parseQuery query]
                                body = "<!doctype html><meta charset=utf-8><title>anima2D</title><body style=\"font:15px -apple-system,sans-serif;display:grid;place-items:center;height:90vh\"><p>Signed in. You can close this tab and go back to anima2D.</p>"
                            NetB.sendAll conn (BS8.pack ("HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: " <> show (BS.length body) <> "\r\nConnection: close\r\n\r\n") <> body)
                            Net.close conn
                            putMVar box pairs
            server <- forkIO (void (try serve :: IO (Either SomeException ())))
            pure (Right (takeMVar box, killThread server >> Net.close sock))

{- | Who signed in, from the ID token's claims, for showing only (the server checks the
access token; this does not).
-}
displayName :: Tokens -> Maybe Text
displayName t = do
    jwt <- t.idToken
    payload <- case Text.splitOn "." jwt of
        [_, p, _] -> Just p
        _ -> Nothing
    raw <- either (const Nothing) Just (B64.decodeUnpadded (Text.encodeUtf8 payload))
    claims <- either (const Nothing) Just (decodeJson raw)
    case claims of
        Aeson.Object o -> foldr (\k acc -> case KeyMap.lookup k o of Just (Aeson.String v) | not (Text.null v) -> Just v; _ -> acc) Nothing ["preferred_username", "email", "name"]
        _ -> Nothing

{- | A signed-in session: the access token now (renewed when it is about to run out), a
forced renewal (after the server refused one), and who is signed in.
-}
data Auth = Auth
    { bearer :: IO (Either Text Text)
    , renew :: IO (Either Text Text)
    , signedInAs :: IO (Maybe Text)
    }

-- | The keychain's name for a provider and client's refresh token.
keychainAccount :: Oidc -> Text
keychainAccount o = "oidc " <> o.issuer <> " " <> o.clientId

{- | The session for `o`, from the refresh token kept in the keychain (or `fresh`, just
signed in). Each renewal keeps the provider's new refresh token.
-}
newAuth :: Http.Manager -> Oidc -> Maybe Tokens -> IO Auth
newAuth manager o fresh = do
    held <- newIORef fresh
    let
        account = keychainAccount o
        keep t = do
            writeIORef held (Just t)
            mapM_ (saveSecret account) t.refreshToken
        renewWith = do
            mine <- readIORef held
            stored <- maybe (loadSecret account) (pure . Just) (mine >>= (.refreshToken))
            case stored of
                Nothing -> pure (Left ("not signed in to " <> o.issuer <> ": sign in under Settings…"))
                Just rt -> do
                    r <- refresh manager o rt
                    case r of
                        Right t -> keep t >> pure (Right t.access)
                        Left why -> do
                            -- A refresh token the provider no longer takes is forgotten.
                            _ <- forgetSecret account
                            writeIORef held Nothing
                            pure (Left ("the sign-in ran out (" <> why <> "): sign in again under Settings…"))
    pure
        Auth
            { bearer = do
                now <- realToFrac <$> getPOSIXTime
                mine <- readIORef held
                case mine of
                    Just t | t.expiresAt - now > 30 -> pure (Right t.access)
                    _ -> renewWith
            , renew = renewWith
            , signedInAs = (>>= displayName) <$> readIORef held
            }
