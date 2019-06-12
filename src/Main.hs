module Main where

import AppSettings
import System.Console.CmdArgs
import Instrument
import Score
import Render

-- parse cli arguments
main = do
    runApp =<< cmdArgs appSettings

-- render mode
runApp (AppSettings Render tuning intsrumentPath scorePath outputPath stereoMode delaySamples) = do
    instruments <- readInstruments $ nopath "instrument" intsrumentPath
    scores <- readScores $ nopath "score" scorePath
    putStrLn "Render mode"
    render instruments scores tuning outputPath stereoMode delaySamples

-- live mode
runApp (AppSettings Live tuning intsrumentPath _ _ stereoMode delaySamples) = do
    instruments <- readInstruments $ nopath "instrument" intsrumentPath
    putStrLn "Live mode"

-- return error if path is empty
nopath param [] = error $ "No path for \"" ++ param ++ "\" specified"
nopath param str = str