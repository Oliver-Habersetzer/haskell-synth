module Main where

import AppSettings
import System.Console.CmdArgs
import Instrument
import Score

-- parse cli arguments
main = runApp =<< cmdArgs appSettings

-- render mode
runApp (AppSettings Render tuning intsrumentPath scorePath outputPath) = do
    instruments <- readInstruments $ nopath "instrument" intsrumentPath
    scores <- readScores $ nopath "score" scorePath
    putStrLn "Render mode"
    putStrLn $ show instruments
    putStrLn $ show scores

-- live mode
runApp (AppSettings Live tuning intsrumentPath _ _) = do
    instruments <- readInstruments $ nopath "instrument" intsrumentPath
    putStrLn "Live mode"
    putStrLn $ show instruments
  
    

-- helper functions
-- return error if path is empty
nopath param [] = error $ "No path for \"" ++ param ++ "\" specified"
nopath param str = str