{-# LANGUAGE ParallelListComp #-}

module Output (
    out
) where
----
import Instrument
import Oscilators
import Score
import Effects
import Data.List
import Data.Int
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as BB
import Data.Binary
import AppSettings
import System.IO
import Data.List.Split
import Data.Char
----
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
-----------------------------------------------
out = do
    let filename = "/data/rendered.wav"

--unterhalb die 1024 auf 512, falls die Soundkarte nicht mehr die beste ist;)
    result <- initAudio 64 44100 1024 -- max channels, mixing frequency, mixing buffer size

    unless result $ fail "failed to initialize the audio system"

    -- (A) load sample from file
 --   sampleA <- sampleFromFile filename 1.0 -- volume

--    soundPlay sampleA 1 1 0 1 -- left volume, right volume, time difference between left and right, pitch factor for playback
--------------------------------------------------------------------------------
 --   waitPayback
 --   let filename = "/home/pz/Schreibtisch/FunktionalProg/Projekt/test.wav"
    -- (B) load from memory buffer
 --   buffer <- SB.readFile filename
--------------------------------------------------------------------------------
    let filename = "/home/pz/Schreibtisch/FunktionalProg/Projekt/testWav.wav"
    -- (B) load from memory buffer
    buffer1 <- SB.readFile filename
    sampleB <- case takeExtension filename of
      ".ogg" -> sampleFromMemoryOgg buffer 1.0
      ".wav" -> sampleFromMemoryWav buffer 1.0 (sampleFromMemoryWav buffer1 1.0)
    soundPlay sampleB 1 1 0 1 -- left volume, right volume, time difference between left and right, pitch factor for playback
    waitPayback


    finishAudio