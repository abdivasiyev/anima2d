{- | mulberry32: the small seeded generator the browser editor uses for scatter stamps and
pencil grain, bit for bit, so the same seed places the same stamps in both editors.
-}
module Anima.Desktop.Random
    ( Gen
    , mulberry32
    , next
    )
where

import Data.Bits (shiftR, xor, (.|.))
import Data.Word (Word32)

newtype Gen = Gen Word32

mulberry32 :: Int -> Gen
mulberry32 seed = Gen (fromIntegral seed)

-- | A number in [0, 1), and the generator after it.
next :: Gen -> (Double, Gen)
next (Gen a0) =
    let
        a = a0 + 0x6d2b79f5
        t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
        t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
        r = t2 `xor` (t2 `shiftR` 14)
    in
        (fromIntegral r / 4294967296, Gen a)
