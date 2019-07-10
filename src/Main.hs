module Main where

import AppSettings
import System.Console.CmdArgs
import Instrument
import Score
import Render
import Live
import Utils
import MidiConverter
import System.IO

-- parse cli arguments
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    runApp =<< cmdArgs appSettings

runApp :: AppSettings -> IO ()

-- render mode
runApp (AppSettings Render tuning intsrumentPath scorePath outputPath stereoMode delaySamples playAfterRender _ loop) = do
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
        render instruments scores tuning outputPath stereoMode delaySamples playAfterRender loop
    else do
        putStrLn "OK"
        scores <- readScores $ nopath "score" scorePath
        render instruments scores tuning outputPath stereoMode delaySamples playAfterRender loop

-- live mode
runApp (AppSettings Live tuning intsrumentPath _ _ stereoMode delaySamples _ defaultInstrument _) = do
    instruments <- readInstruments $ nopath "instrument" intsrumentPath
    live instruments defaultInstrument tuning stereoMode

-- fail if path is empty
nopath :: [Char] -> [a] -> [a]
nopath param [] = fail $ "No path for \"" ++ param ++ "\" specified"
nopath param str = str