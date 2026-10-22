{- | The desktop's ports agree with the browser: stroke outlines match perfect-freehand's
point for point (cases generated from the JavaScript library), what the pen makes
parses, the bucket fills a closed outline, and moving or turning a selection lands where
it should.
-}
module Main (main) where

import Anima.Tools (ToolSpec (..))
import Anima.Types.Parse (Parse (..))
import Anima.Types.Project
import Control.Monad (forM_, unless)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Either (lefts)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, findExecutable, getTemporaryDirectory, listDirectory, removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Map.Strict qualified as Map

import Anima.Desktop.Assistant (Reply (..), Stop (..), ToolOutcome (..), ToolUse (..), anthropicReply, anthropicRequest, anthropicResults, openAIReply, openAIRequest, openAIResults)
import Anima.Desktop.Export (Export (..), Fit (..), Format (..), Quality (..), Size (..), Track (..), exportFilm)
import Anima.Desktop.Make (Ink (..), ShapeTool (..))
import Anima.Desktop.Scene (Env (..), newCache)

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
    let
        wire = wireChecks <> exported
    forM_ (filled <> moved <> wire) putStrLn
    putStrLn (show (length filled + length moved) <> " fill and selection checks failed; " <> show (length wire) <> " assistant wire checks failed")
    unless (null bad && all (either (const False) (const True)) made && null filled && null moved && null wire) exitFailure

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
