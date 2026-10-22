{- | anima2d-desktop. For now: `anima2d-desktop render <project.json> <frame> <width> <out.png>`
draws one frame, the way the window will.
-}
module Main (main) where

import Anima.Types.Project (Project, viewProject)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map

import Anima.Desktop.Scene (renderPng)
import Anima.Desktop.Ui (runApp)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["render", file, frame, width, out]
            | Just f <- readMaybe frame
            , Just w <- readMaybe width -> do
                raw <- LBS.readFile file
                case Aeson.eitherDecode raw :: Either String Project of
                    Left why -> die why
                    Right p -> renderPng Map.empty (viewProject p) f w out
        [] -> runApp
        _ -> die "usage: anima2d-desktop (the window), or anima2d-desktop render <project.json> <frame> <width> <out.png>"
