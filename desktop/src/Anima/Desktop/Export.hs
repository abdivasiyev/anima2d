{- | The film out of the desktop: every frame drawn by the same renderer as the canvas
(through the camera, reference layers left out), at a preset's size, and handed to
ffmpeg as raw pixels, with the audio tracks mixed in. The presets, qualities and
bitrates are the browser's (@web/src/domain/export.ts@), so a film looks the same from
either. PNG frames need no ffmpeg.
-}
module Anima.Desktop.Export
    ( Format (..)
    , formatName
    , formatExtension
    , keepsAlpha
    , Quality (..)
    , Fit (..)
    , Size (..)
    , presets
    , Export (..)
    , Track (..)
    , bitrate
    , exportFilm
    , drawFrameAt
    )
where

import Anima.Types.Project (ProjectView (..))
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, when)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import Numeric (showFFloat)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose, hGetContents, hSetBinaryMode)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, readProcessWithExitCode, terminateProcess, waitForProcess)

import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Text qualified as Text
import GI.Cairo.Render qualified as C

import Anima.Desktop.Scene (Env (..), drawFrame, duration)

data Format = H264 | Hevc | ProRes | Gif | PngFrames
    deriving stock (Bounded, Enum, Eq, Show)

formatName :: Format -> Text
formatName = \case
    H264 -> "MP4 · H.264"
    Hevc -> "MP4 · HEVC"
    ProRes -> "MOV · ProRes 4444"
    Gif -> "GIF"
    PngFrames -> "PNG frames (a folder)"

formatExtension :: Format -> Text
formatExtension = \case
    H264 -> "mp4"
    Hevc -> "mp4"
    ProRes -> "mov"
    Gif -> "gif"
    PngFrames -> ""

-- | Formats that keep a transparent background.
keepsAlpha :: Format -> Bool
keepsAlpha f = f `elem` [ProRes, Gif, PngFrames]

-- | Bitrate against YouTube's recommendation: 1×, 1.5×, 2.5×.
data Quality = Standard | High | Max
    deriving stock (Bounded, Enum, Eq, Show)

-- | When the size's shape differs from the canvas: letterbox it, or fill and crop.
data Fit = Contain | Cover
    deriving stock (Eq, Show)

data Size = Size {sizeName :: Text, width :: Int, height :: Int}
    deriving stock (Eq, Show)

-- | The canvas's own size first, then the browser's presets.
presets :: Int -> Int -> [Size]
presets cw ch =
    Size ("Canvas size (" <> Text.pack (show cw <> " × " <> show ch) <> ")") cw ch
        : [ Size "YouTube 4K" 3840 2160
          , Size "YouTube 2K (1440p)" 2560 1440
          , Size "YouTube 1080p" 1920 1080
          , Size "YouTube 720p" 1280 720
          , Size "Shorts 1080 × 1920" 1080 1920
          , Size "Shorts 4K" 2160 3840
          ]

data Export = Export {format :: Format, size :: Size, quality :: Quality, fit :: Fit, transparent :: Bool}

-- | An audio track to mix: its file, where it starts in frames, how loud.
data Track = Track {file :: FilePath, offset :: Int, volume :: Double}

-- | Video bits per second, as the browser reckons them.
bitrate :: Size -> Int -> Quality -> Int
bitrate s fps q =
    let
        pixels = fromIntegral (s.width * s.height) :: Double
        base
            | pixels >= 3840 * 2160 * 0.9 = 45e6
            | pixels >= 2560 * 1440 * 0.9 = 16e6
            | pixels >= 1920 * 1080 * 0.9 = 8e6
            | otherwise = 5e6 :: Double
        rate = if fps >= 48 then 1.5 else 1
        k = case q of
            Standard -> 1
            High -> 1.5
            Max -> 2.5
    in
        round (base * rate * k)

-- | Frame `t`, at the export's size: the canvas fitted in (letterboxed) or cropped to fill.
drawFrameAt :: Env -> Export -> Int -> IO C.Surface
drawFrameAt env e t = do
    let
        v = env.project
        cw = fromIntegral v.width
        ch = fromIntegral v.height
        w = fromIntegral e.size.width
        h = fromIntegral e.size.height
        k = (if e.fit == Cover then max else min) (w / cw) (h / ch)
        dw = max 1 (round (cw * k)) :: Int
        dh = max 1 (round (ch * k)) :: Int
        white = not (e.transparent && keepsAlpha e.format)
    s <- C.createImageSurface C.FormatARGB32 e.size.width e.size.height
    C.renderWith s $ do
        when white $ do
            C.setSourceRGB 1 1 1
            C.paint
        C.translate ((w - fromIntegral dw) / 2) ((h - fromIntegral dh) / 2)
        drawFrame env t dw dh True white
    C.surfaceFlush s
    pure s

-- | The surface's pixels as straight-alpha BGRA rows, as ffmpeg's rawvideo reads them.
straightBgra :: C.Surface -> Bool -> IO ByteString
straightBgra s opaque = do
    w <- C.imageSurfaceGetWidth s
    h <- C.imageSurfaceGetHeight s
    stride <- C.imageSurfaceGetStride s
    bytes <- C.imageSurfaceGetData s
    let
        row = w * 4
    if opaque && stride == row
        then pure (BS.copy bytes)
        else BSU.unsafeUseAsCString bytes $ \src ->
            BSI.create (row * h) $ \dst ->
                forM_ [0 .. h - 1] $ \y -> forM_ [0 .. w - 1] $ \x -> do
                    let
                        from = src `plusPtr` (y * stride + x * 4) :: Ptr Word8
                        to = dst `plusPtr` (y * row + x * 4)
                    a <- peekByteOff from 3 :: IO Word8
                    forM_ [0, 1, 2] $ \c -> do
                        v <- peekByteOff from c :: IO Word8
                        let
                            u = if a == 0 then 0 else min 255 ((fromIntegral v * 255 + fromIntegral a `div` 2) `div` fromIntegral a) :: Int
                        pokeByteOff to c (fromIntegral (if opaque then fromIntegral v else u) :: Word8)
                    pokeByteOff to 3 (if opaque then 255 else a)

{- | The film to `out` (a folder, for PNG frames). `progress` hears each frame done;
`stop` is asked between frames. ffmpeg is needed for every format but PNG frames.
-}
exportFilm :: Maybe FilePath -> Env -> [Track] -> Export -> FilePath -> (Int -> Int -> IO ()) -> IO Bool -> IO (Either Text ())
exportFilm ffmpeg env tracks e out progress stop = do
    let
        v = env.project
        total = duration v
        fps = max 1 v.rate
    if e.size.width < 16 || e.size.height < 16 || e.size.width > 7680 || e.size.height > 7680
        then pure (Left "expected a size from 16 to 7680 pixels")
        else case (e.format, ffmpeg) of
            (PngFrames, _) -> do
                r <- try $ do
                    createDirectoryIfMissing True out
                    let
                        loop t
                            | t >= total = pure (Right ())
                            | otherwise = do
                                halt <- stop
                                if halt
                                    then pure (Left "stopped")
                                    else do
                                        s <- drawFrameAt env e t
                                        C.surfaceWriteToPNG s (out </> ("frame-" <> pad (t + 1) <> ".png"))
                                        progress (t + 1) total
                                        loop (t + 1)
                    loop 0
                pure (either (\(x :: SomeException) -> Left (Text.pack (show x))) id r)
            (_, Nothing) -> pure (Left "this format needs ffmpeg on the PATH")
            (_, Just exe) -> do
                encoders <- either (\(_ :: SomeException) -> "") (\(_, o, _) -> o) <$> try (readProcessWithExitCode exe ["-hide_banner", "-encoders"] "")
                let
                    has name = Text.pack name `Text.isInfixOf` Text.pack encoders
                    opaque = not (e.transparent && keepsAlpha e.format)
                    rate = bitrate e.size fps e.quality
                    kbits n = show (n `div` 1000) <> "k"
                    video = case e.format of
                        H264
                            | has "h264_videotoolbox" -> ["-c:v", "h264_videotoolbox", "-b:v", kbits rate, "-pix_fmt", "yuv420p"]
                            | otherwise -> ["-c:v", "libx264", "-b:v", kbits rate, "-maxrate", kbits rate, "-bufsize", kbits (rate * 2), "-pix_fmt", "yuv420p", "-preset", "medium"]
                        Hevc
                            | has "hevc_videotoolbox" -> ["-c:v", "hevc_videotoolbox", "-b:v", kbits rate, "-tag:v", "hvc1", "-pix_fmt", "yuv420p"]
                            | otherwise -> ["-c:v", "libx265", "-b:v", kbits rate, "-tag:v", "hvc1", "-pix_fmt", "yuv420p"]
                        ProRes -> ["-c:v", "prores_ks", "-profile:v", "4", "-pix_fmt", if opaque then "yuv444p10le" else "yuva444p10le", "-vendor", "apl0"]
                        Gif -> ["-vf", "split[a][b];[a]palettegen=stats_mode=diff" <> (if opaque then "" else ":reserve_transparent=1") <> "[p];[b][p]paletteuse=dither=bayer:bayer_scale=4", "-loop", "0"]
                    -- Each track delayed (or cut) to its offset and set to its volume, then mixed.
                    sound = e.format `elem` [H264, Hevc, ProRes] && not (null tracks)
                    inputs = concat [["-i", tr.file] | sound, tr <- tracks]
                    shaped =
                        [ "["
                            <> show i
                            <> ":a]"
                            <> (if tr.offset >= 0 then "adelay=" <> show ms <> ":all=1" else "atrim=start=" <> seconds (fromIntegral (negate tr.offset) / fromIntegral fps) <> ",asetpts=PTS-STARTPTS")
                            <> ",volume="
                            <> seconds tr.volume
                            <> "[a"
                            <> show i
                            <> "]"
                        | (i, tr) <- zip [1 :: Int ..] tracks
                        , let
                            ms = tr.offset * 1000 `div` fps
                        ]
                    mix = Text.unpack (Text.intercalate ";" (map Text.pack shaped)) <> ";" <> concat ["[a" <> show i <> "]" | i <- [1 .. length tracks]] <> "amix=inputs=" <> show (length tracks) <> ":normalize=0[mix]"
                    audio
                        | sound = ["-filter_complex", mix, "-map", "0:v", "-map", "[mix]", "-c:a", if e.format == ProRes then "pcm_s16le" else "aac", "-b:a", "192k"]
                        | otherwise = []
                    args =
                        ["-hide_banner", "-loglevel", "error", "-y", "-f", "rawvideo", "-pix_fmt", "bgra", "-s", show e.size.width <> "x" <> show e.size.height, "-r", show fps, "-i", "-"]
                            <> inputs
                            <> video
                            <> audio
                            <> ["-t", seconds (fromIntegral total / fromIntegral fps)]
                            <> (if e.format `elem` [H264, Hevc] then ["-movflags", "+faststart"] else [])
                            <> [out]
                started <- try (createProcess (proc exe args) {std_in = CreatePipe, std_err = CreatePipe})
                case started of
                    Left (x :: SomeException) -> pure (Left ("ffmpeg could not start: " <> Text.pack (show x)))
                    Right (Just pin, _, Just perr, ph) -> do
                        hSetBinaryMode pin True
                        said <- newIORef ""
                        errs <- hGetContents perr
                        halted <- newIORef False
                        wrote <- try $ do
                            let
                                loop t = unless (t >= total) $ do
                                    halt <- stop
                                    if halt
                                        then writeIORef halted True
                                        else do
                                            s <- drawFrameAt env e t
                                            straightBgra s opaque >>= BS.hPut pin
                                            progress (t + 1) total
                                            loop (t + 1)
                            loop 0
                        _ <- try (hClose pin) :: IO (Either SomeException ())
                        h <- readIORef halted
                        when h (terminateProcess ph)
                        code <- waitForProcess ph
                        writeIORef said (take 600 errs)
                        why <- readIORef said
                        pure $ case (wrote, h, code) of
                            (Left (x :: SomeException), _, _) -> Left ("the frames could not be written: " <> Text.pack (show x) <> " " <> Text.pack why)
                            (_, True, _) -> Left "stopped"
                            (_, _, ExitSuccess) -> Right ()
                            (_, _, ExitFailure n) -> Left ("ffmpeg failed (" <> Text.pack (show n) <> "): " <> Text.strip (Text.pack why))
                    Right _ -> pure (Left "ffmpeg could not start")
    where
        pad n = let s = show n in replicate (4 - length s) '0' <> s
        -- ffmpeg reads plain decimals, not 8.3e-2.
        seconds :: Double -> String
        seconds x = showFFloat (Just 4) x ""
