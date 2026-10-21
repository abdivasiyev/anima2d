{- | Generators for projects.

They generate the wire shape (a JSON 'Value') and parse it, so a generated project is
built exactly the way a real one is; nothing here reaches past 'parse'.
-}
module Anima.Gen
    ( genRawProject
    , genProject
    )
where

import Data.Aeson (Value (..), object, (.=))
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import Test.QuickCheck (Gen, choose, elements, listOf1, oneof, vectorOf)

import Data.Text qualified as Text

import Anima.Types.Parse (Parse (..))
import Anima.Types.Project (Project)

genProject :: Gen Project
genProject = either (error . ("generator built an invalid project: " <>) . Text.unpack) id . parse <$> genRawProject

genRawProject :: Gen Value
genRawProject = do
    idLen <- choose (1, 64)
    pid <- Text.pack <$> vectorOf idLen (elements (['a' .. 'z'] <> ['0' .. '9'] <> "-"))
    name <- genName
    w <- (* 2) <$> choose (32, 3840 :: Int)
    h <- (* 2) <$> choose (32, 3840 :: Int)
    fps <- elements [12, 24, 25, 30, 48, 50, 60 :: Int]
    frameCount <- choose (1, 3)
    frames <- vectorOf frameCount genFrame
    pure $ object ["format" .= (2 :: Int), "id" .= pid, "name" .= name, "canvas" .= object ["width" .= w, "height" .= h], "fps" .= fps, "frames" .= frames]

genName :: Gen Text
genName = do
    n <- choose (1, 80)
    t <- Text.strip . Text.pack <$> vectorOf n (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> "ğüşıöç 0123456789"))
    pure (if Text.null t then "Untitled" else t)

genFrame :: Gen Value
genFrame = do
    hold <- choose (1, 24 :: Int)
    n <- choose (0, 4)
    items <- vectorOf n genItem
    pure $ object ["hold" .= hold, "items" .= items]

coord :: Gen Double
coord = choose (-5000, 5000)

vec :: Gen Value
vec = (\x y -> toList [x, y]) <$> coord <*> coord

toList :: [Double] -> Value
toList = Array . foldMap (pure . Number . fromFloatDigits)

genPoints :: Gen [Value]
genPoints = take 8 <$> listOf1 ((\x y p -> toList [x, y, p]) <$> coord <*> coord <*> choose (0, 1))

color :: Gen Text
color = ("#" <>) . Text.pack <$> vectorOf 6 (elements "0123456789abcdef")

layer :: Gen Text
layer = elements ["line", "color"]

size :: Gen Double
size = choose (0.5, 200)

opacity :: Gen Double
opacity = choose (0.05, 1)

genItem :: Gen Value
genItem =
    oneof
        [ do
            (l, b, c, s, o, ps) <- (,,,,,) <$> layer <*> elements ["pen", "pencil", "marker", "paint" :: Text] <*> color <*> size <*> opacity <*> genPoints
            pure $ object ["kind" .= ("stroke" :: Text), "layer" .= l, "brush" .= b, "color" .= c, "size" .= s, "opacity" .= o, "points" .= ps]
        , do
            (s, ps) <- (,) <$> size <*> genPoints
            pure $ object ["kind" .= ("erase" :: Text), "size" .= s, "points" .= ps]
        , do
            (l, b, c, s, o) <- (,,,,) <$> layer <*> elements ["grass", "birds", "stars", "leaves", "flowers", "spray" :: Text] <*> color <*> size <*> opacity
            (seed, ps) <- (,) <$> choose (0, 2147483647 :: Int) <*> genPoints
            pure $ object ["kind" .= ("scatter" :: Text), "layer" .= l, "brush" .= b, "color" .= c, "size" .= s, "opacity" .= o, "seed" .= seed, "points" .= ps]
        , do
            (l, sh, c, s, o) <- (,,,,) <$> layer <*> elements ["line", "rect", "ellipse", "triangle" :: Text] <*> color <*> size <*> opacity
            f <- oneof [pure Null, String <$> color]
            (ctr, w, h, r) <- (,,,) <$> vec <*> choose (0, 200000 :: Double) <*> choose (0, 200000 :: Double) <*> choose (-pi, pi :: Double)
            pure $ object ["kind" .= ("shape" :: Text), "layer" .= l, "shape" .= sh, "color" .= c, "size" .= s, "opacity" .= o, "fill" .= f, "center" .= ctr, "width" .= w, "height" .= h, "rotation" .= r]
        , do
            (l, c, s, o) <- (,,,) <$> layer <*> color <*> size <*> opacity
            ps <- vectorOf 3 vec
            pure $ object ["kind" .= ("curve" :: Text), "layer" .= l, "color" .= c, "size" .= s, "opacity" .= o, "points" .= ps]
        , do
            (l, c, o) <- (,,) <$> layer <*> color <*> opacity
            n <- choose (1, 3)
            polys <- vectorOf n (choose (3, 6) >>= (`vectorOf` vec))
            pure $ object ["kind" .= ("fill" :: Text), "layer" .= l, "color" .= c, "opacity" .= o, "polygons" .= polys]
        ]
