module Playback (
    play,
    loadSamples,
    playSampleLooping
) where

import Control.Monad
import System.Environment
import System.FilePath
import qualified Data.ByteString as SB
import Control.Concurrent
import Sound.ProteaAudio

waitPlayback :: IO ()
waitPlayback = do
    n <- soundActive
    when  (n > 0) $ do
            threadDelay 500000
            waitPlayback
            
loadSamples :: Traversable t => t String -> IO (t Sound.ProteaAudio.Sample)
loadSamples filenames = mapM (\filename -> sampleFromFile filename 1.0) filenames

playSampleLooping :: Sound.ProteaAudio.Sample -> IO ()
playSampleLooping sample = do 
    soundLoop sample 1 1 0 1
    waitPlayback
    finishAudio

play :: String -> Bool -> IO ()
play filename loop = do
    -- max channels, mixing frequency, buffer size
    audioEngine <- initAudio 2 44100 1024
    unless audioEngine $ error "Failed to initialize the audio system"
    -- load sample from file
    sample <- sampleFromFile filename 1.0
    -- left volume, right volume, time difference between left and right, pitch factor for playback
    if loop then do
        soundLoop sample 1 1 0 1
        waitPlayback
        finishAudio
    else do
        soundPlay sample 1 1 0 1
        waitPlayback
        finishAudio