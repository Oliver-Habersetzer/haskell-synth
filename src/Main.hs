module Main where

import AppMode
import System.Console.CmdArgs
import Instrument
import Score

main = runApp =<< cmdArgs appModes

runApp (Render intsrumentPath scorePath outputPath) = do
    instruments <- readInstruments $ nopath "instrument" intsrumentPath
    scores <- readScores $ nopath "score" scorePath
    putStrLn "Render mode"
    putStrLn $ show instruments
    putStrLn $ show scores

runApp (Live intsrumentPath) = do
    instruments <- readInstruments $ nopath "instrument" intsrumentPath
    putStrLn "Live mode"
    putStrLn $ show instruments

nopath param [] = error $ "No path for \"" ++ param ++ "\" specified"
nopath param str = str