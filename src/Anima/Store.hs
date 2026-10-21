{- | Where projects are kept.

A gateway, so a record of functions: the server takes a 'ProjectStore' and the specs pass
one over a temporary directory. The only implementation is 'fileStore', one JSON file per
project plus a small summary beside it so the project list never reads drawings.

What comes back off disk is parsed like anything off the wire: a file edited by hand, or
written by a future format, is a 'Corrupt' error, never a half-valid 'Project'.
-}
module Anima.Store
    ( ProjectStore (..)
    , StoreError (..)
    , fileStore
    , modifyProject
    )
where

import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (IOException, try)
import Data.Aeson (FromJSON)
import Data.Either (partitionEithers)
import Data.List (sortOn)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, renameFile)
import System.FilePath (takeFileName, (</>))
import System.IO (hClose, openTempFile)

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text

import Anima.Types.Parse (Parse (..))
import Anima.Types.Project
    ( Project
    , ProjectId
    , ProjectSummary
    , Revision
    , firstRevision
    , nextRevision
    , projectId
    , summarize
    , summaryName
    , summaryRevision
    )

data StoreError
    = -- | No project has this id.
      NotFound
    | -- | A file on disk does not parse; the text names the file and what was expected.
      Corrupt Text
    | -- | The disk refused; the text is the operation that failed.
      Unavailable Text
    | -- | Someone saved first: the project is now at this revision.
      Conflict Revision
    deriving stock (Eq, Show)

data ProjectStore = ProjectStore
    { list :: IO (Either StoreError [ProjectSummary])
    , load :: ProjectId -> IO (Either StoreError (Project, ProjectSummary))
    , save :: Maybe Revision -> Project -> IO (Either StoreError ProjectSummary)
    {- ^ Save, if the project is still at the revision this edit started from. 'Nothing'
      saves regardless: a new project, or a caller that means to overwrite.
    -}
    }

{- | Projects as @<id>.json@ with @<id>.summary.json@ beside each, under one directory.

Writes go to a temporary file in the same directory and are renamed into place, so a
crash mid-save leaves the previous version, never a truncated one. Saves take a lock, so
the revision check and the write cannot interleave with another save.
-}
fileStore :: FilePath -> IO ProjectStore
fileStore dir = do
    createDirectoryIfMissing True dir
    lock <- newMVar ()
    pure
        ProjectStore
            { list = listSummaries
            , load = loadProject
            , save = \base project -> withMVar lock (const (saveProject base project))
            }
    where
        projectFile pid = dir </> Text.unpack (unparse pid) <> ".json"
        summaryFile pid = dir </> Text.unpack (unparse pid) <> ".summary.json"

        listSummaries = guarded "list projects" $ do
            names <- listDirectory dir
            let
                summaries = [dir </> n | n <- names, ".summary.json" `Text.isSuffixOf` Text.pack n]
            results <- traverse readParsed summaries
            pure $ case partitionEithers results of
                ([], ok) -> Right (sortOn summaryName ok)
                (err : _, _) -> Left err

        loadProject pid = do
            exists <- doesFileExist (projectFile pid)
            if exists
                then do
                    project <- readParsed (projectFile pid)
                    summary <- readParsed (summaryFile pid)
                    pure ((,) <$> project <*> summary)
                else pure (Left NotFound)

        currentRevision pid = do
            exists <- doesFileExist (summaryFile pid)
            if exists then fmap (Just . summaryRevision) <$> readParsed (summaryFile pid) else pure (Right Nothing)

        saveProject base project = guarded "save the project" $ do
            let
                pid = projectId project
            current <- currentRevision pid
            case current of
                Left err -> pure (Left err)
                Right now
                    | Just b <- base, Just n <- now, b /= n -> pure (Left (Conflict n))
                    | otherwise -> do
                        let
                            summary = summarize (maybe firstRevision nextRevision now) project
                        writeAtomically (projectFile pid) (Aeson.encode project)
                        writeAtomically (summaryFile pid) (Aeson.encode summary)
                        pure (Right summary)

        writeAtomically path bytes = do
            (tmp, h) <- openTempFile dir (takeFileName path <> ".tmp")
            LBS.hPut h bytes
            hClose h
            renameFile tmp path

{- | Load, change and save, retrying once if another save lands in between. For callers
that edit a project in small steps (the MCP tools) rather than replace it.
-}
modifyProject :: ProjectStore -> ProjectId -> (Project -> Either Text Project) -> IO (Either (Either StoreError Text) ProjectSummary)
modifyProject store pid f = attempt (2 :: Int)
    where
        attempt n = do
            loaded <- store.load pid
            case loaded of
                Left err -> pure (Left (Left err))
                Right (project, summary) -> case f project of
                    Left why -> pure (Left (Right why))
                    Right changed -> do
                        saved <- store.save (Just (summaryRevision summary)) changed
                        case saved of
                            Left (Conflict _) | n > 1 -> attempt (n - 1)
                            Left err -> pure (Left (Left err))
                            Right s -> pure (Right s)

readParsed :: (FromJSON a) => FilePath -> IO (Either StoreError a)
readParsed path = do
    bytes <- try (LBS.readFile path)
    pure $ case bytes of
        Left (_ :: IOException) -> Left (Unavailable ("read " <> Text.pack (takeFileName path)))
        Right raw -> case Aeson.eitherDecode raw of
            Left why -> Left (Corrupt (Text.pack (takeFileName path) <> ": " <> Text.pack why))
            Right a -> Right a

guarded :: Text -> IO (Either StoreError a) -> IO (Either StoreError a)
guarded what act = do
    result <- try act
    pure $ case result of
        Left (_ :: IOException) -> Left (Unavailable what)
        Right r -> r
