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
    )
where

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
import Anima.Types.Project (Project, ProjectId, ProjectSummary, projectId, summarize, summaryName)

data StoreError
    = -- | No project has this id.
      NotFound
    | -- | A file on disk does not parse; the text names the file and what was expected.
      Corrupt Text
    | -- | The disk refused; the text is the operation that failed.
      Unavailable Text
    deriving stock (Eq, Show)

data ProjectStore = ProjectStore
    { list :: IO (Either StoreError [ProjectSummary])
    , load :: ProjectId -> IO (Either StoreError Project)
    , save :: Project -> IO (Either StoreError ProjectSummary)
    }

{- | Projects as @<id>.json@ with @<id>.summary.json@ beside each, under one directory.

Writes go to a temporary file in the same directory and are renamed into place, so a
crash mid-save leaves the previous version, never a truncated one.
-}
fileStore :: FilePath -> IO ProjectStore
fileStore dir = do
    createDirectoryIfMissing True dir
    pure
        ProjectStore
            { list = listSummaries
            , load = loadProject
            , save = saveProject
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
                then readParsed (projectFile pid)
                else pure (Left NotFound)

        saveProject project = guarded "save the project" $ do
            let
                pid = projectId project
                summary = summarize project
            writeAtomically (projectFile pid) (Aeson.encode project)
            writeAtomically (summaryFile pid) (Aeson.encode summary)
            pure (Right summary)

        writeAtomically path bytes = do
            (tmp, h) <- openTempFile dir (takeFileName path <> ".tmp")
            LBS.hPut h bytes
            hClose h
            renameFile tmp path

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
