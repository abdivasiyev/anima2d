{- | The outline of a pressure-sensitive stroke: a port of perfect-freehand 1.2 (MIT, Steve
Ruiz), the library the browser editor draws strokes with, step for step, so a stroke has
the same shape in both editors. Points in, a closed polygon out.
-}
module Anima.Desktop.Freehand
    ( Options (..)
    , outline
    )
where

import Data.Maybe (fromMaybe)

type V = (Double, Double)

data Options = Options {size :: Double, thinning :: Double, smoothing :: Double, streamline :: Double}

data Sample = Sample {point :: V, pressure :: Double, vector :: V, distance :: Double, runningLength :: Double}

add, sub :: V -> V -> V
add (a, b) (c, d) = (a + c, b + d)
sub (a, b) (c, d) = (a - c, b - d)

mul :: V -> Double -> V
mul (a, b) k = (a * k, b * k)

perp :: V -> V
perp (x, y) = (y, -x)

neg :: V -> V
neg (x, y) = (-x, -y)

dot :: V -> V -> Double
dot (a, b) (c, d) = a * c + b * d

len :: V -> Double
len (x, y) = sqrt (x * x + y * y)

dist :: V -> V -> Double
dist a b = len (sub a b)

dist2 :: V -> V -> Double
dist2 (a, b) (c, d) = (a - c) * (a - c) + (b - d) * (b - d)

unit :: V -> V
unit v = let l = len v in mul v (1 / l)

lerp :: V -> V -> Double -> V
lerp a b t = add a (mul (sub b a) t)

-- | `p` turned by `r` radians about `c`.
rotAround :: V -> V -> Double -> V
rotAround (px, py) (cx, cy) r =
    let
        s = sin r
        c = cos r
        x = px - cx
        y = py - cy
    in
        (x * c - y * s + cx, x * s + y * c + cy)

fixedPi :: Double
fixedPi = pi + 1.0e-4

-- | 0, step, 2·step … while at most `hi`, accumulated the way the JavaScript loops do.
steps :: Double -> Double -> Double -> [Double]
steps from step hi = takeWhile (<= hi) (iterate (+ step) from)

stepsBelow :: Double -> Double -> Double -> [Double]
stepsBelow from step hi = takeWhile (< hi) (iterate (+ step) from)

-- | The points, streamlined, with their direction and running length.
strokePoints :: Options -> [(Double, Double, Double)] -> [Sample]
strokePoints _ [] = []
strokePoints o raw = fix (go first False 0 (zip [1 :: Int ..] (drop 1 pts)) [first])
    where
        s = 0.15 + (1 - o.streamline) * 0.85
        base = case raw of
            -- Two points: four more between them, without a pressure (so 0.5), as the original does.
            [(ax, ay, ap), (bx, by, _)] -> (ax, ay, ap) : [let (x, y) = lerp (ax, ay) (bx, by) (fromIntegral t / 4) in (x, y, -1) | t <- [1 :: Int .. 4]]
            [a@(ax, ay, ap)] -> [a, (ax + 1, ay + 1, ap)]
            xs -> xs
        pts = base
        h = length pts - 1
        pressureOf def pr = if pr >= 0 then pr else def
        first = case pts of
            (x, y, pr) : _ -> Sample {point = (x, y), pressure = pressureOf 0.25 pr, vector = (1, 1), distance = 0, runningLength = 0}
            [] -> Sample (0, 0) 0.25 (1, 1) 0 0
        go _ _ _ [] acc = reverse acc
        go prev reached run ((e, (x, y, pr)) : rest) acc =
            let
                t = if e == h then (x, y) else lerp prev.point (x, y) s
            in
                if t == prev.point
                    then go prev reached run rest acc
                    else
                        let
                            r = dist t prev.point
                            run' = run + r
                        in
                            if e < h && not reached && run' < o.size
                                then go prev reached run' rest acc
                                else
                                    let
                                        m = Sample {point = t, pressure = pressureOf 0.5 pr, vector = unit (sub prev.point t), distance = r, runningLength = run'}
                                    in
                                        go m True run' rest (m : acc)
        fix ss = case ss of
            a : b : more -> a {vector = b.vector} : b : more
            [a] -> [a {vector = (0, 0)}]
            [] -> []

radiusOf :: Options -> Double -> Double
radiusOf o pr = o.size * (0.5 - o.thinning * (0.5 - pr))

-- | The outline polygon of a stroke (the last point taken as given, as the editors do).
outline :: Options -> [(Double, Double, Double)] -> [V]
outline o raw
    | null ps || o.size <= 0 = []
    | length ps == 1 = dotAt (headOr ps).point firstRadius
    | otherwise = reverse leftPts <> endCap <> rightPts <> startCap
    where
        ps = strokePoints o raw
        headOr = \case
            x : _ -> x
            [] -> Sample (0, 0) 0.5 (0, 0) 0 0
        lastS = last ps
        total = lastS.runningLength
        minD = (o.size * o.smoothing) ** 2
        n = length ps
        initial = foldl' (\acc r -> (acc + r.pressure) / 2) (headOr ps).pressure (take 10 ps)
        step0 = St {left = [], right = [], prevPressure = initial, radius = radiusOf o lastS.pressure, firstR = Nothing, prevVec = (headOr ps).vector, pl = (headOr ps).point, pr = (headOr ps).point, sharp = False}
        final = foldl' walk step0 (zip3 [0 :: Int ..] ps (drop 1 ps <> [lastS]))
        firstRadius = fromMaybe final.radius final.firstR
        leftPts = final.left
        rightPts = final.right
        walk st (i, cur, next) =
            let
                isLast = i == n - 1
            in
                if not isLast && total - cur.runningLength < 3
                    then st
                    else
                        let
                            rad0 = if o.thinning /= 0 then radiusOf o cur.pressure else o.size / 2
                            rad = max 0.01 rad0
                            st1 = st {radius = rad, firstR = Just (fromMaybe rad st.firstR)}
                            nextVec = if isLast then cur.vector else next.vector
                            nextDpr = if isLast then 1 else dot cur.vector nextVec
                            cornerHere = dot cur.vector st.prevVec < 0 && not st.sharp
                            cornerNext = nextDpr < 0
                        in
                            if cornerHere || cornerNext
                                then
                                    let
                                        off = mul (perp st.prevVec) rad
                                        ts = steps 0 0.07692307692307693 1
                                        ls = [rotAround (sub cur.point off) cur.point (fixedPi * t) | t <- ts]
                                        rs = [rotAround (add cur.point off) cur.point (fixedPi * (-t)) | t <- ts]
                                    in
                                        st1 {left = reverse ls <> st.left, right = reverse rs <> st.right, pl = lastOr cur.point ls, pr = lastOr cur.point rs, sharp = cornerNext || st.sharp}
                                else
                                    if isLast
                                        then
                                            let
                                                off = mul (perp cur.vector) rad
                                            in
                                                st1 {left = sub cur.point off : st.left, right = add cur.point off : st.right, sharp = False}
                                        else
                                            let
                                                off = mul (perp (lerp nextVec cur.vector nextDpr)) rad
                                                l = sub cur.point off
                                                r = add cur.point off
                                                keepL = i <= 1 || dist2 st.pl l > minD
                                                keepR = i <= 1 || dist2 st.pr r > minD
                                            in
                                                st1
                                                    { left = if keepL then l : st.left else st.left
                                                    , right = if keepR then r : st.right else st.right
                                                    , pl = if keepL then l else st.pl
                                                    , pr = if keepR then r else st.pr
                                                    , prevPressure = cur.pressure
                                                    , prevVec = cur.vector
                                                    , sharp = False
                                                    }
        lastOr d xs = if null xs then d else last xs
        startPoint = (headOr ps).point
        endPoint = lastS.point
        -- Round caps: the start swept from the first right point, the end a half turn about the last.
        startCap = case reverse rightPts of
            r0 : _ -> [rotAround r0 startPoint (fixedPi * t) | t <- steps (1 / 13) (1 / 13) 1]
            [] -> []
        endCap =
            let
                dir = perp (neg lastS.vector)
                start = add endPoint (mul dir final.radius)
            in
                [rotAround start endPoint (fixedPi * 3 * t) | t <- stepsBelow (1 / 29) (1 / 29) 1]
        dotAt c r =
            let
                start = add c (mul (unit (perp (sub c (add c (1, 1))))) (-r))
            in
                [rotAround start c (fixedPi * 2 * t) | t <- steps (1 / 13) (1 / 13) 1]

data St = St
    { left :: [V]
    , right :: [V]
    , prevPressure :: Double
    , radius :: Double
    , firstR :: Maybe Double
    , prevVec :: V
    , pl :: V
    , pr :: V
    , sharp :: Bool
    }
