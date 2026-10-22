{- | The desktop's ports agree with the browser: stroke outlines match perfect-freehand's
point for point (cases generated from the JavaScript library), what the pen makes
parses, the bucket fills a closed outline, and moving or turning a selection lands where
it should.
-}
module Main (main) where

import Anima.Types.Parse (Parse (..))
import Anima.Types.Project
import Control.Monad (forM_, unless)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Either (lefts)
import Data.Text (Text)
import System.Exit (exitFailure)

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map

import Anima.Desktop.Make (Ink (..), ShapeTool (..))

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
    forM_ (filled <> moved) putStrLn
    putStrLn (show (length filled + length moved) <> " fill and selection checks failed")
    unless (null bad && all (either (const False) (const True)) made && null filled && null moved) exitFailure

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
