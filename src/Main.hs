module Main where

import AppMode
import System.Console.CmdArgs

main = runApp =<< cmdArgs appModes

runApp (Render intstrumentPath scorePath outputPath) = do
        putStrLn "RENDER"

runApp (Live intstrumentPath) = do
        putStrLn "LIVE"