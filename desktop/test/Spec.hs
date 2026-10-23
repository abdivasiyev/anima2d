{- | The desktop's ports agree with the browser: stroke outlines match perfect-freehand's
point for point (cases generated from the JavaScript library), what the pen makes
parses, the bucket fills a closed outline, and moving or turning a selection lands where
it should.
-}
module Main (main) where

import Anima.Tools (ToolSpec (..))
import Anima.Types.Parse (Parse (..))
import Anima.Types.Project
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (forM_, join, unless, void)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Either (lefts)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, findExecutable, getTemporaryDirectory, listDirectory, removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Base64.URL qualified as B64
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEnc
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp

import Anima.Desktop.Assistant (Reply (..), Stop (..), ToolOutcome (..), ToolUse (..), anthropicReply, anthropicRequest, anthropicResults, openAIReply, openAIRequest, openAIResults)
import Anima.Desktop.Backend (Backend (..), remoteBackend)
import Anima.Desktop.Config (Connection (..), connectionValue, parseConnection)
import Anima.Desktop.Export (Export (..), Fit (..), Format (..), Quality (..), Size (..), Track (..), exportFilm)
import Anima.Desktop.Make (Ink (..), ShapeTool (..))
import Anima.Desktop.Oidc (Auth (..), Oidc (..), challengeOf, keychainAccount, newAuth, parseDiscovery, parseOidc, parseTokens, signIn)
import Anima.Desktop.Scene (Env (..), newCache)
import Anima.Desktop.Secrets (forgetSecret)

import Anima.Desktop.Fill qualified as Fill
import Anima.Desktop.Freehand qualified as Freehand
import Anima.Desktop.Make qualified as Make

data Case = Case {brush :: String, name :: String, size :: Double, thinning :: Double, smoothing :: Double, streamline :: Double, points :: [[Double]], outline :: [[Double]]}

instance FromJSON Case where
    parseJSON = withObject "case" $ \o -> Case <$> o .: "brush" <*> o .: "name" <*> o .: "size" <*> o .: "thinning" <*> o .: "smoothing" <*> o .: "streamline" <*> o .: "points" <*> o .: "outline"

main :: IO ()
main = do
    raw <- LBS.readFile "test/freehand-cases.json"
    cases <- either (\e -> putStrLn e >> exitFailure) pure (Aeson.eitherDecode raw :: Either String [Case])
    bad <- fmap concat . sequence $ flip map cases $ \c -> do
        let
            opts = Freehand.Options {size = c.size, thinning = c.thinning, smoothing = c.smoothing, streamline = c.streamline}
            got = Freehand.outline opts [(x, y, p) | [x, y, p] <- c.points]
            want = [(x, y) | [x, y] <- c.outline]
            worst = maximum (0 : [sqrt ((a - x) ** 2 + (b - y) ** 2) | ((a, b), (x, y)) <- zip got want])
            ok = length got == length want && worst < 1.0e-6
        unless ok (putStrLn (c.brush <> " " <> c.name <> " size " <> show c.size <> ": " <> show (length got) <> " points, want " <> show (length want) <> ", worst " <> show worst))
        pure [() | not ok]
    let
        ink = Ink {color = "#1c1c1e", size = 6, opacity = 1}
        made =
            [ either (Left . ("stroke: " <>)) (const (Right ())) (Make.stroke "pen" ink [(0, 0, 0.5), (10, 10, 0.7)])
            , either (Left . ("shape: " <>)) (const (Right ())) (Make.shape EllipseShape ink True True (0, 0) (40, 20))
            , either (Left . ("scatter: " <>)) (const (Right ())) (Make.scatterPath "grass" ink 7 [(0, 0, 0.5)])
            , either (Left . ("erase: " <>)) (const (Right ())) (Make.erase 12 [(0, 0, 1)])
            , either (Left . ("text: " <>)) (const (Right ())) (Make.textAt ink "Hello" 48 (10, 10))
            ]
    forM_ (lefts made) print
    putStrLn (show (length cases - length bad) <> " of " <> show (length cases) <> " outlines match; " <> show (length [() | Right () <- made]) <> " of " <> show (length made) <> " items parse")
    filled <- fillChecks ink
    moved <- transformChecks
    exported <- exportChecks
    signedIn <- oidcChecks
    let
        wire = wireChecks <> exported <> signedIn
    forM_ (filled <> moved <> wire) putStrLn
    putStrLn (show (length filled + length moved) <> " fill and selection checks failed; " <> show (length wire) <> " wire, export and sign-in checks failed")
    unless (null bad && all (either (const False) (const True)) made && null filled && null moved && null wire) exitFailure

{- | The Keycloak sign-in: PKCE as RFC 7636 has it, the provider's documents parsed, and the
whole flow against a fake provider (discovery, the browser's way back to the loopback
listener, the code exchange, a refused token renewed and the request sent again).
-}
oidcChecks :: IO [String]
oidcChecks = do
    let
        expect what ok = ["oidc: " <> what | not ok]
        port = 19493
        back = 19494
        iss = "http://127.0.0.1:" <> Text.pack (show port) <> "/realms/test"
        sso = Oidc {issuer = iss, clientId = "spec-client"}
        discovery = Aeson.object ["issuer" Aeson..= iss, "authorization_endpoint" Aeson..= (iss <> "/auth"), "token_endpoint" Aeson..= (iss <> "/token")]
        pure' = [] :: [String]
    challenge <- newIORef ("" :: Text)
    let
        idToken = "e30." <> TextEnc.decodeUtf8 (B64.encodeUnpadded "{\"preferred_username\":\"ada\"}") <> ".sig"
        tokens a r = Aeson.object ["access_token" Aeson..= (a :: Text), "refresh_token" Aeson..= (r :: Text), "expires_in" Aeson..= (60 :: Int), "token_type" Aeson..= ("Bearer" :: Text), "id_token" Aeson..= idToken]
        json v = Wai.responseLBS HttpTypes.status200 [("Content-Type", "application/json")] (Aeson.encode v)
        provider req respond = case (Wai.requestMethod req, Wai.pathInfo req) of
            ("GET", ["realms", "test", ".well-known", "openid-configuration"]) -> respond (json discovery)
            ("GET", ["realms", "test", "auth"]) -> do
                let
                    q k = maybe "" TextEnc.decodeUtf8 (join (lookup k (Wai.queryString req)))
                writeIORef challenge (q "code_challenge")
                if q "client_id" /= "spec-client" || q "code_challenge_method" /= "S256"
                    then respond (Wai.responseLBS HttpTypes.status400 [] "bad request")
                    else respond (Wai.responseLBS HttpTypes.status302 [("Location", TextEnc.encodeUtf8 (q "redirect_uri" <> "?code=c123&state=" <> q "state"))] "")
            ("POST", ["realms", "test", "token"]) -> do
                form <- HttpTypes.parseSimpleQuery . LBS.toStrict <$> Wai.strictRequestBody req
                want <- readIORef challenge
                let
                    f k = maybe "" TextEnc.decodeUtf8 (lookup k form)
                respond $ case f "grant_type" of
                    "authorization_code" | f "code" == "c123" && challengeOf (TextEnc.encodeUtf8 (f "code_verifier")) == want -> json (tokens "at1" "rt1")
                    "refresh_token" | f "refresh_token" `elem` ["rt1", "rt2"] -> json (tokens "at2" "rt2")
                    _ -> Wai.responseLBS HttpTypes.status400 [("Content-Type", "application/json")] "{\"error\":\"invalid_grant\"}"
            ("GET", ["api", "projects"]) ->
                respond
                    $ if lookup "Authorization" (Wai.requestHeaders req) == Just "Bearer at2"
                        then json ([] :: [Aeson.Value])
                        else Wai.responseLBS HttpTypes.status401 [] "expired"
            _ -> respond (Wai.responseLBS HttpTypes.status404 [] "")
    server <- forkIO (Warp.run port provider)
    threadDelay 300000
    manager <- Http.newManager Http.defaultManagerSettings
    -- The browser: the provider's page, then where it sends us (the loopback listener).
    let
        browser url = void . forkIO $ do
            req <- Http.parseRequest (Text.unpack url)
            res <- Http.httpLbs req {Http.redirectCount = 0} manager
            forM_ (lookup "Location" (Http.responseHeaders res)) $ \loc -> do
                back' <- Http.parseRequest (BS8.unpack loc)
                void (Http.httpLbs back' manager)
    flow <- signIn manager sso back browser
    listed <- case flow of
        Right t -> do
            auth <- newAuth manager sso (Just t)
            tmp <- getTemporaryDirectory
            b <- remoteBackend manager (Text.pack ("http://127.0.0.1:" <> show port)) [] (Just auth) tmp
            r <- b.list
            who <- auth.signedInAs
            _ <- forgetSecret (keychainAccount sso)
            pure (Right (either (const Nothing) (Just . length) r, who))
        Left e -> pure (Left e)
    killThread server
    pure
        $ pure'
            <> expect "the PKCE challenge is RFC 7636's" (challengeOf "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk" == "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM")
            <> expect "a provider that names another issuer is refused" (either (const True) (const False) (parseDiscovery "https://other.example.com/realms/x" discovery))
            <> expect "a provider on plain http elsewhere is refused" (either (const True) (const False) (parseOidc (Aeson.object ["issuer" Aeson..= ("http://auth.example.com/realms/a" :: Text), "clientId" Aeson..= ("c" :: Text)])))
            <> expect "a token error is the provider's words" (parseTokens 0 (Aeson.object ["error" Aeson..= ("invalid_grant" :: Text), "error_description" Aeson..= ("Session not active" :: Text)]) == Left "invalid_grant: Session not active")
            <> expect "a connection with a sign-in round-trips" (let c = Remote "https://anim.example.com" [] (Just Oidc {issuer = "https://auth.example.com/realms/anima", clientId = "anima2d-desktop"}) in parseConnection (connectionValue c) == Right c)
            <> expect ("signs in, renews a refused token and lists the projects: " <> show listed) (listed == Right (Just 0, Just "ada"))

-- | The bouncing ball out as each format, checked with ffprobe (skipped without ffmpeg).
exportChecks :: IO [String]
exportChecks = do
    ffmpeg <- findExecutable "ffmpeg"
    ffprobe <- findExecutable "ffprobe"
    raw <- LBS.readFile "../examples/bouncing-ball.anima2d.json"
    case (ffmpeg, ffprobe, Aeson.eitherDecode raw >>= either (Left . show) Right . parse @Project) of
        (Nothing, _, _) -> putStrLn "skip the export checks: no ffmpeg on the PATH" >> pure []
        (_, Nothing, _) -> putStrLn "skip the export checks: no ffprobe on the PATH" >> pure []
        (_, _, Left e) -> pure ["export: " <> e]
        (Just exe, Just probe, Right p) -> do
            tmp <- getTemporaryDirectory
            let
                dir = tmp </> "anima2d-export-spec"
                small = Size "test" 320 180
                job f t = Export {format = f, size = small, quality = Standard, fit = Contain, transparent = t}
                v = viewProject p
            createDirectoryIfMissing True dir
            cache <- newCache
            let
                env = Env {pictures = Map.empty, cache = cache, project = v}
                tone = dir </> "tone.wav"
                run f t out tracks = exportFilm (Just exe) env tracks (job f t) (dir </> out) (\_ _ -> pure ()) (pure False)
                streams out = do
                    (_, o, _) <- readProcessWithExitCode probe ["-v", "error", "-count_frames", "-show_entries", "stream=codec_type,width,height,pix_fmt,nb_read_frames", "-of", "csv=p=0", dir </> out] ""
                    pure (lines o)
            _ <- readProcessWithExitCode exe ["-loglevel", "error", "-y", "-f", "lavfi", "-i", "sine=frequency=440:duration=1", tone] ""
            h264 <- run H264 False "ball.mp4" [Track tone 6 0.5]
            prores <- run ProRes True "ball.mov" []
            gif <- run Gif False "ball.gif" []
            pngs <- run PngFrames False "frames" []
            s1 <- streams "ball.mp4"
            s2 <- streams "ball.mov"
            s3 <- streams "ball.gif"
            framesOut <- length . filter (".png" `isSuffixOf`) <$> listDirectory (dir </> "frames")
            removeDirectoryRecursive dir
            let
                n = show (duration p)
                expect what ok = ["export: " <> what | not ok]
            pure
                $ expect ("MP4 written: " <> show h264) (h264 == Right ())
                    <> expect ("MP4 has the frames and the tone: " <> show s1) (any (\l -> "video,320,180" `isInfixOf` l && ("," <> n) `isSuffixOf` l) s1 && any ("audio" `isPrefixOf`) s1)
                    <> expect ("ProRes keeps the transparency: " <> show (prores, s2)) (prores == Right () && any ("yuva444p" `isInfixOf`) s2)
                    <> expect ("GIF has every frame: " <> show (gif, s3)) (gif == Right () && any (("," <> n) `isSuffixOf`) s3)
                    <> expect ("PNG frames, one a frame: " <> show (pngs, framesOut)) (pngs == Right () && show framesOut == n)

-- | The assistant's requests and replies, both providers, as their APIs define them.
wireChecks :: [String]
wireChecks =
    let
        tools = [ToolSpec "add_items" "Draw items" [("drawing", Aeson.object ["type" Aeson..= ("integer" :: Text)])] ["drawing"]]
        history = [Aeson.object ["role" Aeson..= ("user" :: Text), "content" Aeson..= ("Draw a sun" :: Text)]]
        req = anthropicRequest "claude-opus-5-5" "system" tools history
        thinking = Aeson.object ["type" Aeson..= ("thinking" :: Text), "thinking" Aeson..= ("" :: Text), "signature" Aeson..= ("sig" :: Text)]
        reply =
            Aeson.object
                [ "stop_reason" Aeson..= ("tool_use" :: Text)
                , "content"
                    Aeson..= [ thinking
                             , Aeson.object ["type" Aeson..= ("text" :: Text), "text" Aeson..= ("A sun, then." :: Text)]
                             , Aeson.object ["type" Aeson..= ("tool_use" :: Text), "id" Aeson..= ("toolu_1" :: Text), "name" Aeson..= ("add_items" :: Text), "input" Aeson..= Aeson.object ["drawing" Aeson..= (1 :: Int)]]
                             ]
                ]
        refusal = Aeson.object ["stop_reason" Aeson..= ("refusal" :: Text), "content" Aeson..= ([] :: [Aeson.Value]), "stop_details" Aeson..= Aeson.object ["explanation" Aeson..= ("no" :: Text)]]
        results = anthropicResults [ToolOutcome "toolu_1" "Frame 1" (Just "PNG") False, ToolOutcome "toolu_2" "bad" Nothing True]
        oaReply =
            Aeson.object
                [ "choices"
                    Aeson..= [ Aeson.object
                                [ "finish_reason" Aeson..= ("tool_calls" :: Text)
                                , "message" Aeson..= Aeson.object ["role" Aeson..= ("assistant" :: Text), "content" Aeson..= Aeson.Null, "tool_calls" Aeson..= [Aeson.object ["id" Aeson..= ("call_1" :: Text), "type" Aeson..= ("function" :: Text), "function" Aeson..= Aeson.object ["name" Aeson..= ("add_items" :: Text), "arguments" Aeson..= ("{\"drawing\":2}" :: Text)]]]]
                                ]
                             ]
                ]
        oaResults = openAIResults [ToolOutcome "call_1" "Frame 1" (Just "PNG") False]
        field k = \case
            Aeson.Object o -> KeyMap.lookup k o
            _ -> Nothing
        expect what ok = ["assistant wire: " <> what | not ok]
    in
        expect "the request names the model, tools with input_schema and the fallback" (field "model" req == Just "claude-opus-5-5" && field "fallbacks" req == Just "default" && maybe False (\case Aeson.Array ts -> all (isJust . field "input_schema") ts && not (null ts); _ -> False) (field "tools" req))
            <> case anthropicReply reply of
                Right r -> expect "a tool_use reply asks for its tool and keeps every block (thinking too)" (r.stop == WantsTools && map (.name) r.calls == ["add_items"] && r.said == "A sun, then." && fmap (\case Aeson.Array bs -> length bs; _ -> 0) (field "content" r.keep) == Just 3)
                Left e -> ["assistant wire: " <> show e]
            <> expect "a refusal is a decline" (either (const False) ((== Declined "no") . (.stop)) (anthropicReply refusal))
            <> expect
                "tool results go back in one user turn, the picture as an image block"
                ( case results of
                    [Aeson.Object o] -> KeyMap.lookup "role" o == Just "user" && maybe False (\case Aeson.Array rs -> length rs == 2; _ -> False) (KeyMap.lookup "content" o) && "\"media_type\":\"image/png\"" `isInfixOf` LBS8.unpack (Aeson.encode (Aeson.Object o)) && "\"is_error\":true" `isInfixOf` LBS8.unpack (Aeson.encode (Aeson.Object o))
                    _ -> False
                )
            <> expect "an OpenAI-style tool call is read, its arguments decoded" (either (const False) (\r -> r.stop == WantsTools && map (.input) r.calls == [Aeson.object ["drawing" Aeson..= (2 :: Int)]]) (openAIReply oaReply))
            <> expect "OpenAI-style results: a tool message, then the picture from the user" (length oaResults == 2)
            <> expect
                "the OpenAI request puts the system prompt first"
                ( case field "messages" (openAIRequest "m" "sys" tools history) of
                    Just (Aeson.Array ms) | m : _ <- foldr (:) [] ms -> field "role" m == Just "system"
                    _ -> False
                )

-- | A click inside an outlined box fills the box, and no further than its line.
fillChecks :: Ink -> IO [String]
fillChecks ink = case Make.shape RectShape ink False False (100, 80) (300, 220) >>= parsedView of
    Left e -> pure ["fill: " <> show e]
    Right box -> do
        r <- Fill.rasterOf Map.empty 400 300 [box]
        pure $ case Fill.traceFill r 200 150 4 maxPointsPerFill of
            Left e -> ["fill: " <> show e]
            Right polys ->
                let
                    ps = concat polys
                    (xs, ys) = (map fst ps, map snd ps)
                    near a b = abs (a - b) <= 6
                in
                    [ "fill: expected the box's inside, got " <> show (minimum xs, minimum ys, maximum xs, maximum ys)
                    | null ps || not (near (minimum xs) 100 && near (maximum xs) 300 && near (minimum ys) 80 && near (maximum ys) 220)
                    ]
                        <> ["fill: " <> show e | Left e <- [Make.fillArea ink polys]]

-- | An item, as the window sees it once it is in a project.
parsedView :: Item -> Either Text ItemView
parsedView = Right . viewItem

-- | Moving the bouncing ball's drawing by (10, 20), and turning it a quarter about the origin.
transformChecks :: IO [String]
transformChecks = do
    raw <- LBS.readFile "../examples/bouncing-ball.anima2d.json"
    pure $ case Aeson.eitherDecode raw >>= either (Left . show) Right . parse @Project of
        Left e -> ["transform: " <> e]
        Right p ->
            let
                items q = case (viewProject q).stack of
                    _ : LayerView {kind = DrawingView _ (c : _)} : _ -> c.items
                    _ -> []
                after m = either (const []) items (transformItemsAt 1 1 [0, 2] m p)
                firstPoint = \case
                    StrokeV s : _ -> case s.points of
                        (x, y, _) : _ -> Just (x, y)
                        [] -> Nothing
                    _ -> Nothing
                centreOf = \case
                    ShapeV s : _ -> Just s.center
                    _ -> Nothing
                strokes = filter (\case StrokeV _ -> True; _ -> False)
                close (a, b) (c, d) = abs (a - c) < 1.0e-6 && abs (b - d) < 1.0e-6
                moved = after (1, 0, 0, 1, 10, 20)
                turned = after (0, 1, -1, 0, 0, 0)
                check what f q = case (f (items p), f q) of
                    (Just (x, y), Just got) | close (want what x y) got -> []
                    (was, got) -> ["transform: " <> what <> ": " <> show was <> " became " <> show got]
                want what x y = if take 4 what == "move" then (x + 10, y + 20) else (-y, x)
            in
                check "move shape" centreOf moved
                    <> check "move stroke" (firstPoint . strokes) moved
                    <> check "turn shape" centreOf turned
                    <> check "turn stroke" (firstPoint . strokes) turned
                    <> ["transform: the unselected shape moved" | centreOf (drop 1 (items p)) /= centreOf (drop 1 moved)]
