{- | Stills from reference videos, for the board and for reference layers: ffmpeg grabs the
frame at a moment, in the background, into the cache folder; once read, it is kept in
memory too. GTK has no video of its own here, and a still a frame is all drawing to a
video needs.
-}
module Anima.Desktop.Video
    ( Frames
    , newFrames
    , available
    , ffmpegPath
    , frameAt
    , lengthOf
    , step
    )
where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, renameFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import GI.Cairo.Render qualified as C

-- | How finely a video is cut into stills: thirty a second.
step :: Double
step = 1 / 30

data Frames = Frames
    { ffmpeg :: Maybe FilePath
    , folder :: FilePath
    , stills :: IORef (Map.Map (Text, Int) C.Surface)
    , pending :: IORef (Set.Set (Text, Int))
    , lengths :: IORef (Map.Map Text (Maybe Double))
    }

-- | ffmpeg from ANIMA2D_FFMPEG or the PATH; stills in `cacheDir`.
newFrames :: FilePath -> IO Frames
newFrames cacheDir = do
    named <- lookupEnv "ANIMA2D_FFMPEG"
    found <- maybe (findExecutable "ffmpeg") (pure . Just) named
    let
        dir = cacheDir </> "frames"
    createDirectoryIfMissing True dir
    Frames found dir <$> newIORef Map.empty <*> newIORef Set.empty <*> newIORef Map.empty

-- | The ffmpeg found, for the export.
ffmpegPath :: Frames -> Maybe FilePath
ffmpegPath f = f.ffmpeg

available :: Frames -> Bool
available f = isJust f.ffmpeg

{- | The still of video `asset` (at `path`) nearest `seconds`, if it is ready; otherwise it is
asked for, and `ready` runs (on some thread) once it is.
-}
frameAt :: Frames -> Text -> FilePath -> Double -> IO () -> IO (Maybe C.Surface)
frameAt f asset path seconds ready = do
    let
        n = max 0 (round (seconds / step)) :: Int
        key = (asset, n)
        file = f.folder </> (Text.unpack asset <> "-" <> show n <> ".png")
    have <- Map.lookup key <$> readIORef f.stills
    case (have, f.ffmpeg) of
        (Just s, _) -> pure (Just s)
        (Nothing, Nothing) -> pure Nothing
        (Nothing, Just exe) -> do
            fresh <- atomicModifyIORef' f.pending (\ps -> if Set.member key ps then (ps, False) else (Set.insert key ps, True))
            if not fresh
                then pure Nothing
                else do
                    void . forkIO $ do
                        cached <- doesFileExist file
                        ok <-
                            if cached
                                then pure True
                                else do
                                    let
                                        part = file <> ".part.png"
                                    r <- try (readProcessWithExitCode exe ["-loglevel", "error", "-y", "-ss", show (fromIntegral n * step), "-i", path, "-frames:v", "1", "-vf", "scale='min(1280,iw)':-2", part] "")
                                    case r of
                                        Right (ExitSuccess, _, _) -> do
                                            made <- doesFileExist part
                                            if made then renameFile part file >> pure True else pure False
                                        Right _ -> pure False
                                        Left (_ :: SomeException) -> pure False
                        surface <- if ok then either (\(_ :: SomeException) -> Nothing) Just <$> try (C.imageSurfaceCreateFromPNG file) else pure Nothing
                        atomicModifyIORef' f.stills (\m -> (maybe m (\s -> Map.insert key s m) surface, ()))
                        atomicModifyIORef' f.pending (\ps -> (Set.delete key ps, ()))
                        ready
                    pure Nothing

-- | How long video `asset` runs, in seconds, once ffmpeg has said; asked for the first time.
lengthOf :: Frames -> Text -> FilePath -> IO () -> IO (Maybe Double)
lengthOf f asset path ready = do
    known <- Map.lookup asset <$> readIORef f.lengths
    case (known, f.ffmpeg) of
        (Just l, _) -> pure l
        (Nothing, Nothing) -> pure Nothing
        (Nothing, Just exe) -> do
            atomicModifyIORef' f.lengths (\m -> (Map.insert asset Nothing m, ()))
            void . forkIO $ do
                r <- try (readProcessWithExitCode exe ["-hide_banner", "-i", path] "")
                let
                    -- ffmpeg names no output, so it fails; the length is in what it says first.
                    said = either (\(_ :: SomeException) -> "") (\(_, _, err) -> err) r
                    seconds = case Text.breakOn "Duration: " (Text.pack said) of
                        (_, rest) | not (Text.null rest) -> case Text.splitOn ":" (Text.takeWhile (/= ',') (Text.drop 10 rest)) of
                            [hh, mm, ss] -> (\a b c -> a * 3600 + b * 60 + c) <$> num hh <*> num mm <*> num ss
                            _ -> Nothing
                        _ -> Nothing
                    num t = case reads (Text.unpack (Text.strip t)) of
                        [(v, "")] -> Just (v :: Double)
                        _ -> Nothing
                atomicModifyIORef' f.lengths (\m -> (Map.insert asset seconds m, ()))
                ready
            pure Nothing
