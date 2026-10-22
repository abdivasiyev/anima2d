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
import Control.Monad (when)
import Data.Aeson (FromJSON)
import Data.Either (partitionEithers)
import Data.List (sort, sortOn)
import Data.Text (Text)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import GHC.Conc (TVar, atomically, newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeFile, renameDirectory, renameFile)
import System.FilePath (takeFileName, (</>))
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)

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
    , remove :: ProjectId -> IO (Either StoreError ())
    -- ^ Take a project out of the list: its files move to @trash/@, recoverable by hand.
    , undo :: ProjectId -> IO (Either StoreError (Maybe ProjectSummary))
    {- ^ Bring back the version before the latest change, as a new revision (so open
      editors pick it up like any save); 'Nothing' when there is no older version kept.
    -}
    , redo :: ProjectId -> IO (Either StoreError (Maybe ProjectSummary))
    -- ^ Take back the last 'undo'; 'Nothing' when nothing was undone since the last edit.
    , changes :: Int -> IO Int
    {- ^ Wait (up to 'changeWait') for a save after the one numbered @after@, and answer
      the number of the latest. Saves are counted from 0 when the server starts, so a
      number from before a restart (larger than any now) is answered at once.
    -}
    }

-- | Versions kept per project for undo, the latest first to go when full.
historyLimit :: Int
historyLimit = 100

{- | How long a caller waits for a save before hearing "nothing new": well inside Warp's
and a proxy's timeouts, so the connection is never cut under it.
-}
changeWait :: Int
changeWait = 20_000_000

{- | Projects as @<id>.json@ with @<id>.summary.json@ beside each, under one directory.

Writes go to a temporary file in the same directory and are renamed into place, so a
crash mid-save leaves the previous version, never a truncated one. Saves take a lock, so
the revision check and the write cannot interleave with another save.
-}
fileStore :: FilePath -> IO ProjectStore
fileStore dir = do
    createDirectoryIfMissing True dir
    lock <- newMVar ()
    counter <- newTVarIO (0 :: Int)
    pure
        ProjectStore
            { list = listSummaries
            , load = loadProject
            , save = \base project -> withMVar lock $ \() -> do
                r <- saveProject base project
                -- A new edit: what was undone can no longer be redone.
                either (const (pure ())) (const (forgetUndo (projectId project))) r
                counted counter r
            , remove = \pid -> withMVar lock $ \() -> guarded "delete the project" $ do
                exists <- doesFileExist (projectFile pid)
                if not exists
                    then pure (Left NotFound)
                    else do
                        stamp <- formatTime defaultTimeLocale "%Y%m%dT%H%M%S" <$> getCurrentTime
                        let
                            bin = dir </> "trash" </> (Text.unpack (unparse pid) <> "-" <> stamp)
                        createDirectoryIfMissing True bin
                        renameFile (projectFile pid) (bin </> "project.json")
                        renameFile (summaryFile pid) (bin </> "summary.json")
                        hasHistory <- doesDirectoryExist (historyDir pid)
                        when hasHistory (renameDirectory (historyDir pid) (bin </> "history"))
                        counted counter (Right ())
            , undo = \pid -> withMVar lock $ \() -> guarded "undo" $ do
                st <- readUndo pid
                now <- fmap (fmap (unparse . summaryRevision . snd)) (loadProject pid)
                case now of
                    Left e -> pure (Left e)
                    Right current -> do
                        kept <- keptRevisions pid
                        let
                            from = maybe current fst st
                        case [r | r <- kept, r < from] of
                            [] -> pure (Right Nothing)
                            older -> do
                                let
                                    target = maximum older
                                r <- restoreVersion pid target
                                writeUndo pid target (from : maybe [] snd st)
                                fmap Just <$> counted counter r
            , redo = \pid -> withMVar lock $ \() -> guarded "redo" $ do
                st <- readUndo pid
                case st of
                    Just (_, target : rest) -> do
                        r <- restoreVersion pid target
                        writeUndo pid target rest
                        fmap Just <$> counted counter r
                    _ -> pure (Right Nothing)
            , changes = \after -> do
                news <- timeout changeWait $ atomically $ do
                    n <- readTVar counter
                    if n /= after then pure n else retry
                maybe (readTVarIO counter) pure news
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
                        keep project summary
                        pure (Right summary)

        historyDir pid = dir </> "history" </> Text.unpack (unparse pid)
        undoFile pid = historyDir pid </> "undo.json"

        -- Every save is news for whoever waits, whichever project it was.
        counted :: TVar Int -> Either StoreError a -> IO (Either StoreError a)
        counted tv r = do
            either (const (pure ())) (const (atomically (readTVar tv >>= writeTVar tv . (+ 1)))) r
            pure r

        -- Each saved version under history/<id>/<revision>.json, the oldest pruned.
        keep project summary = do
            let
                pid = projectId project
            createDirectoryIfMissing True (historyDir pid)
            writeAtomically (historyDir pid </> show (unparse (summaryRevision summary)) <> ".json") (Aeson.encode project)
            kept <- keptRevisions pid
            mapM_ (\r -> removeFile (historyDir pid </> show r <> ".json")) (take (length kept - historyLimit) kept)

        keptRevisions :: ProjectId -> IO [Int]
        keptRevisions pid = do
            has <- doesDirectoryExist (historyDir pid)
            names <- if has then listDirectory (historyDir pid) else pure []
            pure (sort [n | name <- names, Just base <- [Text.stripSuffix ".json" (Text.pack name)], Right n <- [readInt base]])

        readInt t = case reads (Text.unpack t) of
            [(n, "")] -> Right (n :: Int)
            _ -> Left ()

        -- Where undo has got to: the version shown, and the ones redo brings back, newest first.
        readUndo :: ProjectId -> IO (Maybe (Int, [Int]))
        readUndo pid = do
            has <- doesFileExist (undoFile pid)
            raw <- if has then Aeson.decode <$> LBS.readFile (undoFile pid) else pure Nothing
            pure (raw >>= \(at, rs) -> if at >= 0 && all (>= 0) rs then Just (at, rs) else Nothing)

        writeUndo pid at rs = writeAtomically (undoFile pid) (Aeson.encode (at, rs))

        forgetUndo pid = do
            has <- doesFileExist (undoFile pid)
            when has (removeFile (undoFile pid))

        -- A kept version saved again as the newest revision.
        restoreVersion pid r = do
            old <- readParsed (historyDir pid </> show r <> ".json")
            case old of
                Left e -> pure (Left e)
                Right project -> saveProject Nothing project

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
