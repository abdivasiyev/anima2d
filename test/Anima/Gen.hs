{- | Generators for the domain types.

They generate the wire shape and parse it, so a generated value is built exactly the way
a real one is; nothing here reaches past 'parse'.
-}
module Anima.Gen
    ( genRawProject
    , genProject
    , parsedOrDiscard
    )
where

import Data.Text (Text)
import Test.QuickCheck (Gen, choose, elements, listOf1, vectorOf)

import Data.Text qualified as Text

import Anima.Types.Parse (Parse (..))
import Anima.Types.Project (Project, RawCanvas (..), RawFrame (..), RawProject (..), RawStroke (..))

parsedOrDiscard :: (Parse a, Show (Raw a)) => Raw a -> a
parsedOrDiscard raw = either (\why -> error ("generator built an invalid value: " <> Text.unpack why <> " from " <> show raw)) id (parse raw)

genProject :: Gen Project
genProject = parsedOrDiscard <$> genRawProject

genRawProject :: Gen RawProject
genRawProject = do
    idLen <- choose (1, 64)
    pid <- Text.pack <$> vectorOf idLen (elements (['a' .. 'z'] <> ['0' .. '9'] <> "-"))
    name <- genName
    w <- (* 2) <$> choose (32, 3840)
    h <- (* 2) <$> choose (32, 3840)
    fps <- elements [12, 24, 25, 30, 48, 50, 60]
    frameCount <- choose (1, 4)
    frames <- vectorOf frameCount genFrame
    pure RawProject {format = 1, id = pid, name, canvas = RawCanvas {width = w, height = h}, fps, frames}

genName :: Gen Text
genName = do
    n <- choose (1, 80)
    t <- Text.strip . Text.pack <$> vectorOf n (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> "ğüşıöç 0123456789"))
    pure (if Text.null t then "Untitled" else t)

genFrame :: Gen RawFrame
genFrame = do
    hold <- choose (1, 24)
    n <- choose (0, 3)
    strokes <- vectorOf n genStroke
    pure RawFrame {hold, strokes}

genStroke :: Gen RawStroke
genStroke = do
    tool <- elements ["pen", "eraser"]
    color <- ("#" <>) . Text.pack <$> vectorOf 6 (elements "0123456789abcdef")
    size <- choose (0.5, 200)
    points <- listOf1 ((,,) <$> choose (-5000, 5000) <*> choose (-5000, 5000) <*> choose (0, 1))
    pure RawStroke {tool, color, size, points}
