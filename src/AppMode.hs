{-# LANGUAGE DeriveDataTypeable #-}

module AppMode (
    appModes,
    AppMode (..)
) where

import System.Console.CmdArgs

appModes = modes
        [
            Live {
                intstrumentPath = ""
                    &= help "Input file for instrument settings (.json-instr)"
            },
            Render {
                intstrumentPath = ""
                    &= help "Input file for instrument settings (.json-instr)",
                scorePath = ""
                    &= help "Input file for score settings (.json-score)",
                outputPath = "./render.raw"
                    &= opt "./render.raw"
                    &= help "Output audio file. Will be mono 16 bit/sample 44100 sample/sec raw format with integer samples"
            }
        ]
        &= summary "Haskell project"

data AppMode
    = Render {
        intstrumentPath :: String,
        scorePath :: String,
        outputPath :: String
    }
    | Live {
        intstrumentPath :: String
    }
    deriving (Show, Data, Typeable)