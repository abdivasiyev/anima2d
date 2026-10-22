module Anima.StoreSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (generate)

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text qualified as Text

import Anima.Gen (genProject)
import Anima.Store (ProjectStore (..), StoreError (..), fileStore)
import Anima.Types.Parse (Parse (..))
import Anima.Types.Project (Project, firstRevision, nextRevision, projectId, summarize, summaryRevision)

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

    it "wakes a caller waiting for changes as soon as something is saved"
        $ withSystemTempDirectory "anima2d"
        $ \dir -> do
            store <- fileStore dir
            project <- generate genProject
            now <- store.changes (-1)
            now `shouldBe` 0
            heardBack <- newEmptyMVar
            _ <- forkIO (store.changes now >>= putMVar heardBack)
            _ <- store.save Nothing project
            heard <- timeout 2_000_000 (takeMVar heardBack)
            heard `shouldBe` Just 1

    it "undoes and redoes saves as new revisions, and forgets redo after an edit"
        $ withSystemTempDirectory "anima2d"
        $ \dir -> do
            store <- fileStore dir
            first <- generate genProject
            second <- renamed first "Second"
            third <- renamed first "Third"
            let
                pid = projectId first
                shown = fmap fst <$> store.load pid
            _ <- store.save Nothing first
            _ <- store.save Nothing second
            undone <- store.undo pid
            fmap (fmap summaryRevision) undone `shouldBe` Right (Just (nextRevision (nextRevision firstRevision)))
            shown >>= (`shouldBe` Right first)
            _ <- store.redo pid
            shown >>= (`shouldBe` Right second)
            _ <- store.undo pid
            _ <- store.save Nothing third
            again <- store.redo pid
            again `shouldBe` Right Nothing

    it "moves a deleted project out of the list"
        $ withSystemTempDirectory "anima2d"
        $ \dir -> do
            store <- fileStore dir
            project <- generate genProject
            _ <- store.save Nothing project
            gone <- store.remove (projectId project)
            gone `shouldBe` Right ()
            listed <- store.list
            listed `shouldBe` Right []
            loaded <- store.load (projectId project)
            fmap fst loaded `shouldBe` Left NotFound

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

-- | The same project under another name, through its wire form (the type is abstract).
renamed :: Project -> Text.Text -> IO Project
renamed p name = case unparse p of
    Aeson.Object o -> either (fail . Text.unpack) pure (parse (Aeson.Object (KeyMap.insert "name" (Aeson.String name) o)))
    _ -> fail "expected an object"
