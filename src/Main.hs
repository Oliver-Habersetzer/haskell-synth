module Main where

import AppSettings
import System.Console.CmdArgs
import Instrument
import Score
import Render
import Utils
import MidiConverter
import System.IO

-- parse cli arguments
main = do
    runApp =<< cmdArgs appSettings

-- render mode
runApp (AppSettings Render tuning intsrumentPath scorePath outputPath stereoMode delaySamples playAfterRender) = do
    putStrLn "Render mode"
    instruments <- readInstruments $ nopath "instrument" intsrumentPath
    
    -- convert midi to score if necessary
    if (isExts scorePath ["mid", "midi"]) then do
        putStrLn "Converting MIDI to score... "
        let outScorePath = scorePath ++ ".json"
        scores <- convertMidiToScoreFile scorePath outScorePath
        writeScore outScorePath scores
        putStrLn $ "\n!!! Conversion done: The converted score file has been saved to " ++ outScorePath ++ " !!!\n"
        -- scores <- readScores $ nopath "score" outScorePath
        render instruments scores tuning outputPath stereoMode delaySamples playAfterRender
    else do
        putStrLn "OK"
        scores <- readScores $ nopath "score" scorePath
        render instruments scores tuning outputPath stereoMode delaySamples playAfterRender


-- live mode
runApp (AppSettings Live tuning intsrumentPath _ _ stereoMode delaySamples _) = do
    instruments <- readInstruments $ nopath "instrument" intsrumentPath
    putStrLn "Live mode"

-- fail if path is empty
nopath param [] = fail $ "No path for \"" ++ param ++ "\" specified"
nopath param str = str