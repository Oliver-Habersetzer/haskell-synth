import Control.Monad
import System.Environment
import System.FilePath
import qualified Data.ByteString as SB
import Control.Concurrent

import Sound.ProteaAudio

waitPayback = do
  n <- soundActive
  when  (n > 0) $ do
    threadDelay 100
    waitPayback
-----------------
-- /home/pz/Schreibtisch/FunktionalProg/Projekt/test.wav
--test Zeugs...nicht groß beachten
name :: IO String
name = do 
  putStrLn"Vorname?"
  vn <- getLine
  putStrLn"Ciao"
  return (vn)
--ende Test Zeugs
----------------------------------------
 -- FilePath ->-- 
--main :: IO ()
main = do
--    args <- getArgs
--    filename <- case args of
--      a : _ -> pure a
--      _ -> fail "usage: proteaaudio-play SAMPLE_FILE_NAME"
    let filename = "/home/pz/Schreibtisch/FunktionalProg/Projekt/test.wav"

    result <- initAudio 64 44100 1024 -- max channels, mixing frequency, mixing buffer size

    unless result $ fail "failed to initialize the audio system"

    -- (A) load sample from file
    sampleA <- sampleFromFile filename 1.0 -- volume

    soundPlay sampleA 1 1 0 1 -- left volume, right volume, time difference between left and right, pitch factor for playback

 --   waitPayback

    -- (B) load from memory buffer
    buffer <- SB.readFile filename
    sampleB <- case takeExtension filename of
      ".ogg" -> sampleFromMemoryOgg buffer 1.0
      ".wav" -> sampleFromMemoryWav buffer 1.0

    soundPlay sampleB 1 1 0 1 -- left volume, right volume, time difference between left and right, pitch factor for playback
 --   waitPayback

    
    finishAudio