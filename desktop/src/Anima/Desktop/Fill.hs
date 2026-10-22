{- | The paint bucket, as the browser's (@web/src/editor/fill.ts@): the line art rasterised
small, its lines thickened by the gap so a nearly closed outline still holds the paint,
the area flood-filled from the click, grown back under the lines, traced with marching
squares, and simplified until it fits the fill's point budget. Holes stay outlines, and
the fill renders even-odd, so an island inside the area stays unpainted.
-}
module Anima.Desktop.Fill
    ( Raster (..)
    , fillRaster
    , rasterOf
    , traceFill
    )
where

import Anima.Types.Project (ItemView, Plane (..))
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, (.|.))
import Data.List (find)
import Data.Text (Text)
import Data.Word (Word8)

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import GI.Cairo.Render qualified as C

import Anima.Desktop.Draw (Pictures, drawItems)

-- | The longest side the line art is rasterised at: plenty for a fill, quick to trace.
fillRaster :: Int
fillRaster = 1600

data Raster = Raster {width :: Int, height :: Int, walls :: U.Vector Word8, scale :: Double}

-- | The drawing's line plane, small, as walls (1 where the line art is).
rasterOf :: Pictures -> Int -> Int -> [ItemView] -> IO Raster
rasterOf pics cw ch items = do
    let
        k = fromIntegral fillRaster / fromIntegral (max cw ch)
        w = max 1 (round (fromIntegral cw * k))
        h = max 1 (round (fromIntegral ch * k))
    s <- C.createImageSurface C.FormatA8 w h
    C.renderWith s $ do
        C.scale k k
        drawItems pics Nothing LinePlane items
    C.surfaceFlush s
    stride <- C.imageSurfaceGetStride s
    bytes <- C.imageSurfaceGetData s
    let
        wallAt i = let (y, x) = i `divMod` w in if BS.index bytes (y * stride + x) > 64 then 1 else 0
    pure Raster {width = w, height = h, walls = U.generate (w * h) wallAt, scale = k}

-- | Square dilation by `r` pixels: two passes of a sliding maximum.
dilate :: Int -> Int -> Int -> U.Vector Word8 -> U.Vector Word8
dilate w h r src
    | r <= 0 = src
    | otherwise = runST $ do
        tmp <- MU.replicate (w * h) 0
        forM_ [0 .. h - 1] $ \y -> sweep (\x -> src U.! (y * w + x)) w (\x -> MU.write tmp (y * w + x) 1)
        tmp' <- U.freeze tmp
        out <- MU.replicate (w * h) 0
        forM_ [0 .. w - 1] $ \x -> sweep (\y -> tmp' U.! (y * w + x)) h (\y -> MU.write out (y * w + x) 1)
        U.freeze out
    where
        -- Along one line of `n`: mark every cell within `r` of a set one.
        sweep :: (Int -> Word8) -> Int -> (Int -> ST s ()) -> ST s ()
        sweep get n mark = go (-r) (minBound `div` 2)
            where
                go x run
                    | x >= n = pure ()
                    | otherwise = do
                        let
                            ahead = x + r
                            run' = if ahead < n && get ahead == 1 then ahead else run
                        when (x >= 0 && run' >= x - r) (mark x)
                        go (x + 1) run'

-- | 4-connected flood fill of the open pixels from (sx, sy).
flood :: Int -> Int -> U.Vector Word8 -> Int -> Int -> U.Vector Word8
flood w h wallsV sx sy = runST $ do
    region <- MU.replicate (w * h) 0
    MU.write region (sy * w + sx) 1
    let
        go [] = pure ()
        go (i : rest) = do
            let
                (y, x) = i `divMod` w
                nbrs = [i - 1 | x > 0] <> [i + 1 | x < w - 1] <> [i - w | y > 0] <> [i + w | y < h - 1]
            fresh <-
                fmap concat
                    . mapM
                        ( \j -> do
                            r <- MU.read region j
                            if r == 0 && wallsV U.! j == 0 then MU.write region j 1 >> pure [j] else pure []
                        )
                    $ nbrs
            go (fresh <> rest)
    go [sy * w + sx]
    U.freeze region

type XY = (Double, Double)

-- | Marching squares: every boundary of the region, outer and holes, as closed loops.
contours :: Int -> Int -> U.Vector Word8 -> [[XY]]
contours w h m = loops
    where
        at x y = if x < 0 || y < 0 || x >= w || y >= h then 0 else fromIntegral (m U.! (y * w + x)) :: Int
        segsOf i j =
            let
                code = (at i j `shiftL` 3) .|. (at (i + 1) j `shiftL` 2) .|. (at (i + 1) (j + 1) `shiftL` 1) .|. at i (j + 1)
                top = (fromIntegral i + 1, fromIntegral j + 0.5)
                right = (fromIntegral i + 1.5, fromIntegral j + 1)
                bottom = (fromIntegral i + 1, fromIntegral j + 1.5)
                left = (fromIntegral i + 0.5, fromIntegral j + 1)
            in
                case code of
                    1 -> [(left, bottom)]
                    14 -> [(left, bottom)]
                    2 -> [(bottom, right)]
                    13 -> [(bottom, right)]
                    3 -> [(left, right)]
                    12 -> [(left, right)]
                    4 -> [(top, right)]
                    11 -> [(top, right)]
                    6 -> [(top, bottom)]
                    9 -> [(top, bottom)]
                    7 -> [(top, left)]
                    8 -> [(top, left)]
                    5 -> [(top, right), (left, bottom)]
                    10 -> [(top, left), (bottom, right)]
                    _ -> []
        segs = [s | j <- [-1 .. h - 1], i <- [-1 .. w - 1], s <- segsOf i j]
        key (x, y) = (round (x * 2) :: Int, round (y * 2) :: Int)
        indexed = zip [0 :: Int ..] segs
        ends = Map.fromListWith (flip (<>)) [(key p, [sid]) | (sid, (a, b)) <- indexed, p <- [a, b]]
        segAt = Map.fromList indexed
        loops = walk Map.empty indexed
        walk _ [] = []
        walk used ((sid, (a, b)) : rest)
            | Map.member sid used = walk used rest
            | otherwise =
                let
                    (loop, used') = follow (Map.insert sid () used) (key a) b [a]
                in
                    (if length loop >= 3 then (reverse loop :) else id) (walk used' rest)
        follow used start here acc
            | key here == start = (acc, used)
            | otherwise =
                case find (\i -> not (Map.member i used)) (Map.findWithDefault [] (key here) ends) of
                    Nothing -> (here : acc, used)
                    Just next ->
                        let
                            (a, b) = Map.findWithDefault (here, here) next segAt
                            there = if key a == key here then b else a
                        in
                            follow (Map.insert next () used) start there (here : acc)

-- | Ramer–Douglas–Peucker on a closed loop, as the browser splits it.
simplify :: Double -> [XY] -> [XY]
simplify epsilon lp =
    let
        half = length lp `div` 2
        initOf xs = take (length xs - 1) xs
    in
        initOf (rdp (take (half + 1) lp)) <> initOf (rdp (drop half lp <> take 1 lp))
    where
        rdp pts
            | length pts < 3 = pts
            | otherwise =
                let
                    a@(ax, ay) = head' pts
                    b@(bx, by) = last pts
                    dx = bx - ax
                    dy = by - ay
                    len = let l = sqrt (dx * dx + dy * dy) in if l == 0 then 1 else l
                    dist (px, py) = abs (dy * px - dx * py + bx * ay - by * ax) / len
                    (worst, at) = foldl (\(wd, wi) (i, p) -> let d = dist p in if d > wd then (d, i) else (wd, wi)) (0, 0) (zip [1 ..] (take (length pts - 2) (drop 1 pts)))
                in
                    if worst <= epsilon then [a, b] else let l = rdp (take (at + 1) pts) in take (length l - 1) l <> rdp (drop at pts)
        head' = \case
            x : _ -> x
            [] -> (0, 0)

area :: [XY] -> Double
area lp = abs (sum [(bx + ax) * (by - ay) | ((ax, ay), (bx, by)) <- zip lp (drop (length lp - 1) lp <> lp)] / 2)

{- | The polygons a click at canvas (x, y) fills, in canvas pixels; `gap` is in canvas
pixels. Outlines are simplified harder until they fit `maxPoints`.
-}
traceFill :: Raster -> Double -> Double -> Double -> Int -> Either Text [[XY]]
traceFill r x y gap maxPoints
    | sx < 0 || sy < 0 || sx >= r.width || sy >= r.height = Left "Click inside the canvas to fill"
    | wallsV U.! (sy * r.width + sx) == 1 = Left "Click inside an outlined area to fill"
    | otherwise =
        let
            region = dilate r.width r.height (g + 1) (flood r.width r.height wallsV sx sy)
            lps = filter ((>= 2) . area) (contours r.width r.height region)
            fits eps = let simple = filter ((>= 3) . length) (map (simplify eps) lps) in if sum (map length simple) <= maxPoints && length simple <= 500 then Just simple else Nothing
        in
            case [s | eps <- takeWhile (< 64) (iterate (* 1.6) 0.6), Just s <- [fits eps]] of
                simple : _ -> Right [[(px / r.scale, py / r.scale) | (px, py) <- lp] | lp <- simple]
                [] -> Right []
    where
        sx = floor (x * r.scale)
        sy = floor (y * r.scale)
        g = round (gap * r.scale)
        wallsV = dilate r.width r.height g r.walls
