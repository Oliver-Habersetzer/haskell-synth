{-# LANGUAGE DeriveDataTypeable #-}

module AppSettings (
    appSettings,
    AppSettings (..),
    AppMode (..),
    TwoChannelMode (..)
) where

import System.Console.CmdArgs

appSettings :: AppSettings
appSettings = AppSettings {
            mode = Render
                &= typ "OPTIONS"
                &= help "Apllication mode options:\n- live\n- render (default)",
            tuning = 440
                &= opt "440"
                &= typ "DECIMAL"
                &= help "Tuning base frequency for A4 in Hz (default: 440 Hz)",
            intstrumentPath = ""
                &= typ "PATH"
                &= help "Input file for instrument settings (.json)",
            scorePath = ""
                &= typ "PATH"
                &= help "Input file for score settings (.json)",
            outputPath = "./rendered.wav"
                &= typ "PATH"
                &= help "Output audio file. Will be mono 16 bit/sample 44100 sample/sec raw format with integer samples",
            twoChannelMode = Mono
                &= name "t"
                &= typ "OPTIONS"
                &= help "Stereo mode options:\n- mono (default)       one channel\n- invert               invert left channel (may result in bass-less audio on mono speakers)\n- delay (recomended)   delay left channel n samples (see delay samples option)",
            delaySamples = 20
                &= typ "INTEGER"
                &= help "Sample count to delay the left channel by (requires '-t=delay' option)"
        }
        &= summary "Haskell project"

data AppSettings = AppSettings {
            mode :: AppMode,
            tuning :: Double,
            intstrumentPath :: String,
            scorePath :: String,
            outputPath :: String,
            twoChannelMode :: TwoChannelMode,
            delaySamples :: Int
        } deriving (Show, Data, Typeable)

data TwoChannelMode = Mono | Invert | Delay deriving (Show, Data, Typeable)

data AppMode = Render | Live deriving (Show, Data, Typeable)