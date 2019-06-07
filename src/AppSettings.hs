{-# LANGUAGE DeriveDataTypeable #-}

module AppSettings (
    appSettings,
    AppSettings (..),
    AppMode (..)
) where

import System.Console.CmdArgs

appSettings :: AppSettings
appSettings = AppSettings {
            mode = Render
                &= help "Apllication mode - options:\n- live\n- render (default)",
            tuning = 440
                &= help "Tuning base frequency for A4 in Hz (default: 440 Hz)",
            intstrumentPath = ""
                &= help "Input file for instrument settings (.json-instr)",
            scorePath = ""
                &= help "Input file for score settings (.json-score)",
            outputPath = "./render.raw"
                &= opt "./render.raw"
                &= help "Output audio file. Will be mono 16 bit/sample 44100 sample/sec raw format with integer samples"
        }
        &= summary "Haskell project"

data AppSettings = AppSettings {
            mode :: AppMode,
            tuning :: Double,
            intstrumentPath :: String,
            scorePath :: String,
            outputPath :: String
        } deriving (Show, Data, Typeable)

data AppMode = Render | Live deriving (Show, Data, Typeable)