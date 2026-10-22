{- | The cases in fixtures/, shared with the editor's suite (web/src/domain/project.test.ts):
both sides must accept, upgrade and reject alike, word for word.
-}
module Anima.FixtureSpec (spec) where

import Data.Aeson (Value (..))
import Data.Text (Text)
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as Text

import Anima.Types.Parse (Parse (..))
import Anima.Types.Project (Project)

load :: FilePath -> IO Value
load name = Aeson.eitherDecodeFileStrict ("fixtures/" <> name) >>= either (fail . (("fixtures/" <> name <> ": ") <>)) pure

spec :: Spec
spec = describe "shared fixtures" $ do
    v1 <- runIO (load "project-v1.json")
    v1Upgraded <- runIO (load "project-v1-upgraded.json")
    v2 <- runIO (load "project-v2.json")
    v2Upgraded <- runIO (load "project-v2-upgraded.json")
    v3 <- runIO (load "project-v3.json")
    v3rig <- runIO (load "project-v3-rig.json")
    v3looks <- runIO (load "project-v3-looks.json")
    rejections <- runIO (load "rejections.json")

    it "a format 3 project with layers, a camera and audio round-trips"
        $ (unparse <$> parse @Project v3) `shouldBe` Right v3
    it "a rigged layer with a posed drawing round-trips"
        $ (unparse <$> parse @Project v3rig) `shouldBe` Right v3rig
    it "blends, clipping, layer motion and a palette round-trip"
        $ (unparse <$> parse @Project v3looks) `shouldBe` Right v3looks
    it "a format 2 project with every kind of item upgrades to format 3"
        $ (unparse <$> parse @Project v2) `shouldBe` Right v2Upgraded
    it "a format 1 project upgrades to format 3"
        $ (unparse <$> parse @Project v1) `shouldBe` Right v1Upgraded

    case rejections of
        Array cases -> mapM_ rejection cases
        _ -> it "rejections.json is a list" (expectationFailure "expected a list")
    where
        rejection = \case
            Object c
                | Just (String name) <- KeyMap.lookup "name" c
                , Just input <- KeyMap.lookup "input" c
                , Just (String expected) <- KeyMap.lookup "error" c ->
                    it ("rejects: " <> Text.unpack name) $ (unparse <$> parse @Project input) `shouldBe` Left (expected :: Text)
            _ -> it "a malformed case" (expectationFailure "expected {name, input, error}")
