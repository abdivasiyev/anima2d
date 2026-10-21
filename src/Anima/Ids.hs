-- | Random ids, for projects an agent creates and for uploaded files.
module Anima.Ids (newUuid) where

import Data.Bits ((.&.), (.|.))
import Data.List (intercalate)
import Numeric (showHex)
import System.IO (IOMode (..), withBinaryFile)

import Data.ByteString qualified as BS
import Data.Text qualified as Text

-- | A random (version 4) UUID, lower-case.
newUuid :: IO Text.Text
newUuid = do
    bytes <- BS.unpack <$> withBinaryFile "/dev/urandom" ReadMode (`BS.hGet` 16)
    let
        -- Version 4 in byte 6, the RFC 4122 variant in byte 8.
        v4 = [if j == 6 then (b .&. 0x0f) .|. 0x40 else if j == 8 then (b .&. 0x3f) .|. 0x80 else b | (j, b) <- zip [0 :: Int ..] bytes]
        hex = concatMap (\b -> let h = showHex b "" in if length h == 1 then '0' : h else h) v4
        (a, r1) = splitAt 8 hex
        (b', r2) = splitAt 4 r1
        (c, r3) = splitAt 4 r2
        (d, e) = splitAt 4 r3
    pure (Text.pack (intercalate "-" [a, b', c, d, e]))
