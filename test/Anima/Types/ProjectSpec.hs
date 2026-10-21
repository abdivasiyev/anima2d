{-# LANGUAGE AllowAmbiguousTypes #-}

module Anima.Types.ProjectSpec (spec) where

import Data.Either (isLeft)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (forAll, (===))

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text

import Anima.Gen (genProject, genRawProject)
import Anima.Types.Parse (Parse (..))
import Anima.Types.Project (Project, ProjectId, ProjectName)

spec :: Spec
spec = do
    describe "Project" $ do
        it "parse . unparse is Right"
            $ forAll genProject
            $ \p -> parse (unparse p) === Right p
        it "writes back exactly the format 2 it read"
            $ forAll genRawProject
            $ \raw -> (unparse <$> parse @Project raw) === Right raw
        it "survives JSON"
            $ forAll genProject
            $ \p -> Aeson.eitherDecode (Aeson.encode p) === Right p

    describe "ProjectId" $ do
        it "accepts a UUID" $ parses @ProjectId "0b7c7a8e-4f1e-4b8e-9d2a-3c6f1e2d4a5b"
        it "rejects upper case, slashes and the empty id"
            $ mapM_ (rejects @ProjectId) ["ABC", "../etc", "", Text.replicate 65 "a"]

    describe "ProjectName" $ do
        it "is trimmed" $ (unparse <$> parse @ProjectName "  Walk cycle ") `shouldBe` Right "Walk cycle"
        it "rejects blank and control characters" $ mapM_ (rejects @ProjectName) ["   ", "a\nb", Text.replicate 81 "x"]
        it "counts length as the editor does, in UTF-16 units" $ rejects @ProjectName (Text.replicate 41 "😀")
        it "never repeats the input in a rejection"
            $ case parse @ProjectName (Text.replicate 81 "secret") of
                Left why -> why `shouldSatisfy` (not . Text.isInfixOf "secret")
                Right _ -> fail "expected a rejection"
    where
        parses :: forall a. (Parse a, Raw a ~ Text) => Text -> IO ()
        parses t = (unparse <$> parse @a t) `shouldBe` Right t
        rejects :: forall a. (Parse a, Raw a ~ Text) => Text -> IO ()
        rejects t = isLeft (parse @a t) `shouldBe` True
