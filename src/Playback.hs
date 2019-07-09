module Playback (
    play,
    loadSamples,
    playSample,
    initPlayback,
    finishPlayback,
    stopSamples
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

playSample :: Sound.ProteaAudio.Sample -> Bool -> Bool -> IO ()
playSample sample loop wait = do 
    if loop then do
        soundLoop sample 1 1 0 1
    else do
        soundPlay sample 1 1 0 1
    if wait then do
        waitPlayback
    else do
        return ()

initPlayback = do
    audioEngine <- initAudio 1024 44100 32
    unless audioEngine $ error "Failed to initialize the audio system"

stopSamples = soundStopAll

finishPlayback = finishAudio

play :: String -> Bool -> IO ()
play filename loop = do
    -- max channels, mixing frequency, buffer size
    initPlayback
    -- load sample from file
    sample <- sampleFromFile filename 1.0
    -- left volume, right volume, time difference between left and right, pitch factor for playback
    playSample sample loop True
    finishAudio