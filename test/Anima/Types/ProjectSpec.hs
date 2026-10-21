{-# LANGUAGE AllowAmbiguousTypes #-}

module Anima.Types.ProjectSpec (spec) where

import Data.Either (isLeft)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (forAll, property, (===))

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text

import Anima.Gen (genProject, genRawProject)
import Anima.Types.Parse (Parse (..))
import Anima.Types.Project
    ( BrushSize
    , Color
    , Fps
    , Height
    , Hold
    , Pressure
    , Project
    , ProjectId
    , ProjectName
    , RawFrame (..)
    , RawProject (..)
    , RawStroke (..)
    , Tool
    , Width
    , fpsValue
    )

spec :: Spec
spec = do
    describe "Project" $ do
        it "parse . unparse is Right"
            $ forAll genProject
            $ \p -> parse (unparse p) === Right p
        it "survives JSON"
            $ forAll genProject
            $ \p -> Aeson.eitherDecode (Aeson.encode p) === Right p
        it "rejects an unknown format"
            $ forAll genRawProject
            $ \raw -> isLeft (parse @Project raw {format = 2})
        it "rejects a project with no frames"
            $ forAll genRawProject
            $ \raw -> isLeft (parse @Project raw {frames = []})
        it "names where a nested rejection happened"
            $ forAll genRawProject
            $ \raw ->
                let
                    bad = raw {frames = [RawFrame {hold = 1, strokes = [RawStroke {tool = "brush", color = "#000000", size = 4, points = [(0, 0, 1)]}]}]}
                in
                    parse @Project bad === Left "frames: 0: strokes: 0: tool: expected a tool of \"pen\" or \"eraser\""

    describe "ProjectId" $ do
        it "accepts a UUID" $ parses @ProjectId "0b7c7a8e-4f1e-4b8e-9d2a-3c6f1e2d4a5b"
        it "rejects upper case, slashes and the empty id"
            $ mapM_ (rejects @ProjectId) ["ABC", "../etc", "", Text.replicate 65 "a"]

    describe "ProjectName" $ do
        it "is trimmed" $ (unparse <$> parse @ProjectName "  Walk cycle ") `shouldBe` Right "Walk cycle"
        it "rejects blank and control characters" $ mapM_ (rejects @ProjectName) ["   ", "a\nb", Text.replicate 81 "x"]

    describe "Fps" $ do
        it "round-trips every rate" $ mapM_ (\f -> parse (unparse f) `shouldBe` Right f) [minBound .. maxBound :: Fps]
        it "rejects others" $ mapM_ (\n -> parse @Fps n `shouldSatisfy` isLeft) [0, 23, 29, 120]
        it "knows 24" $ (fpsValue <$> parse @Fps 24) `shouldBe` Right 24

    describe "Width and Height" $ do
        it "accept 4K" $ do
            (unparse <$> parse @Width 3840) `shouldBe` Right 3840
            (unparse <$> parse @Height 2160) `shouldBe` Right 2160
        it "reject odd and out-of-range sizes" $ do
            mapM_ (\n -> parse @Width n `shouldSatisfy` isLeft) [1919, 62, 7682]
            mapM_ (\n -> parse @Height n `shouldSatisfy` isLeft) [1081, 0, -2]

    describe "drawing values" $ do
        it "Color is kept lower-case" $ (unparse <$> parse @Color "#A1B2C3") `shouldBe` Right "#a1b2c3"
        it "Color rejects other spellings" $ mapM_ (rejects @Color) ["a1b2c3", "#abc", "#gggggg"]
        it "Tool round-trips" $ mapM_ (\t -> parse (unparse t) `shouldBe` Right t) [minBound .. maxBound :: Tool]
        it "Pressure is 0 to 1" $ property $ \d -> isLeft (parse @Pressure d) === (d < 0 || d > 1 || isNaN d)
        it "BrushSize is 0.5 to 200" $ mapM_ (\d -> parse @BrushSize d `shouldSatisfy` isLeft) [0, 0.4, 201, 1 / 0]
        it "Hold is 1 to 24" $ mapM_ (\n -> parse @Hold n `shouldSatisfy` isLeft) [0, 25]

    describe "rejections"
        $ it "never repeat the input"
        $ case parse @ProjectName (Text.replicate 81 "secret") of
            Left why -> why `shouldSatisfy` (not . Text.isInfixOf "secret")
            Right _ -> fail "expected a rejection"
    where
        parses :: forall a. (Parse a, Raw a ~ Text) => Text -> IO ()
        parses t = (unparse <$> parse @a t) `shouldBe` Right t
        rejects :: forall a. (Parse a, Raw a ~ Text) => Text -> IO ()
        rejects t = isLeft (parse @a t) `shouldBe` True
