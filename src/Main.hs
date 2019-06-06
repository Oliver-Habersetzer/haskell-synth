module Main where

import AppMode
import System.Console.CmdArgs
import Instrument

main = runApp =<< cmdArgs appModes

runApp (Render intsrumentPath scorePath outputPath) = do
    putStrLn "RENDER"

runApp (Live intsrumentPath) = do
    instrument <- readInstruments intsrumentPath
    putStrLn $ show instrument