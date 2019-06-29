module Playback (
    play
) where

import Control.Monad
import System.Environment
import System.FilePath
import qualified Data.ByteString as SB
import Control.Concurrent
import Sound.ProteaAudio

waitPayback = do
    n <- soundActive
    when  (n > 0) $ do
            threadDelay 500000
            waitPayback
            
play filename = do
    -- max channels, mixing frequency, buffer size
    audioEngine <- initAudio 2 44100 1024
    unless audioEngine $ error "Failed to initialize the audio system"
    -- load sample from file
    sample <- sampleFromFile filename 1.0
    -- left volume, right volume, time difference between left and right, pitch factor for playback
    soundPlay sample 1 1 0 1
    waitPayback
    finishAudio