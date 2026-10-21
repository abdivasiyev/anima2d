module Anima.StoreSpec (spec) where

import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (generate)

import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text qualified as Text

import Anima.Gen (genProject)
import Anima.Store (ProjectStore (..), StoreError (..), fileStore)
import Anima.Types.Parse (Parse (..))
import Anima.Types.Project (firstRevision, nextRevision, projectId, summarize, summaryRevision)

spec :: Spec
spec = describe "fileStore" $ do
    it "loads what it saved and lists its summary"
        $ withSystemTempDirectory "anima2d"
        $ \dir -> do
            store <- fileStore dir
            project <- generate genProject
            saved <- store.save Nothing project
            saved `shouldBe` Right (summarize firstRevision project)
            loaded <- store.load (projectId project)
            fmap fst loaded `shouldBe` Right project
            listed <- store.list
            listed `shouldBe` Right [summarize firstRevision project]

    it "answers NotFound for an id it never saw"
        $ withSystemTempDirectory "anima2d"
        $ \dir -> do
            store <- fileStore dir
            pid <- either (fail . show) pure (parse "missing")
            result <- store.load pid
            result `shouldBe` Left NotFound

    it "reports a hand-edited file as corrupt rather than loading it"
        $ withSystemTempDirectory "anima2d"
        $ \dir -> do
            store <- fileStore dir
            project <- generate genProject
            _ <- store.save Nothing project
            LBS.writeFile (dir </> Text.unpack (unparse (projectId project)) <> ".json") "{\"format\": 2}"
            result <- store.load (projectId project)
            result `shouldSatisfy` \case
                Left (Corrupt _) -> True
                _ -> False

    it "refuses a save based on a revision someone else already replaced"
        $ withSystemTempDirectory "anima2d"
        $ \dir -> do
            store <- fileStore dir
            project <- generate genProject
            _ <- store.save Nothing project
            first <- store.save (Just firstRevision) project
            fmap summaryRevision' first `shouldBe` Right (nextRevision firstRevision)
            stale <- store.save (Just firstRevision) project
            stale `shouldBe` Left (Conflict (nextRevision firstRevision))
    where
        summaryRevision' = summaryRevision
