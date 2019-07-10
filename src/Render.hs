{-# LANGUAGE ParallelListComp #-}

module Render (
    render,
    renderKey
) where

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
import Utils
import Playback
import Key

clamp :: (Ord p, Num p) => p -> p
clamp x
    | x < -1 = -1
    | x > 1 = 1
    | otherwise = x

sigmoid :: Floating a => a -> a
sigmoid x = 2.0 / (1.0 + exp (negate x)) - 1.0

--remap :: Fractional a => a -> a -> a -> a -> a -> a
remap x in_min in_max out_min out_max = (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min

data InternalTrack = InternalTrack {
        beatsPerMinute :: Double,
        beatsPerBar :: Int,
        divisionsPerBeat :: Int,
        scores :: [InternalScore],
        trackFx :: [Effect],
        outputPath :: String
    } deriving (Show)

data InternalScore = InternalScore {
        name :: String,
        notes :: [InternalNote],
        scoreFx :: [Effect],
        oscilator :: Oscilator
    } deriving (Show)

toTime :: (Double, Double, Double) -> Double -> Double -> Double -> Double
toTime (br, bt, dv) bpm bpb dpb = do
    let _totalDivisions = dv + dpb * bt + dpb * bpb * br
    let _divisionsPerMinute = bpm * dpb
    let _min = _totalDivisions / _divisionsPerMinute
    let _sec = 60.0 * _min
    _sec

toTimedNote :: InternalNote -> Double -> Double -> Double -> (Double, Double, Double, Double, Double, Double)
toTimedNote (InternalNote sF eF sV eV sBr eBr sBt eBt sDv eDv) bpm bpb dpb = (
        -- freq
        sF, eF,
        -- volume
        sV, eV,
        -- time (in sec)
        toTime (fromIntegral sBr, fromIntegral sBt, fromIntegral sDv) bpm bpb dpb,
        toTime (fromIntegral eBr, fromIntegral eBt, fromIntegral eDv) bpm bpb dpb
    )

renderKey :: Oscilator -> Key -> Double -> IO ()
renderKey osc key tuning = do
    let freq = keyToFreq key tuning
    let cycleCount = floor $ freq / 4 :: Integer
    let cycleLength = 1.0 / freq
    let _sampleCount = ceiling ((fromIntegral cycleCount) * cycleLength * 44100) :: Integer
    let _baseTimes = baseTimes _sampleCount
    let rendered = map (\x -> atPhase osc (x * freq) 0.5) _baseTimes
    let outFileName = keyPath key
    putStrLn $ outFileName
    putStrLn $ "  Freq: " ++ (show freq) ++ " Hz"
    putStrLn $ "  Cycle length: " ++ (show cycleLength) ++ " s"
    putStrLn $ "  Cycle count: " ++ (show cycleCount)
    putStrLn $ "  Sample count: " ++ (show _sampleCount)
    putStr "Rendering... "
    writeToWav outFileName 1 (channelToSamples rendered)
    putStrLn "Done"

baseTimes :: (Fractional b, Integral a) => a -> [b]
baseTimes sampleCount = map (\s -> (fromIntegral s) / 44100.0) [0..sampleCount]

writeToWav :: FilePath -> Int16 -> [Int16] -> IO ()
writeToWav outputPath chC flatSamples = do
        B.writeFile outputPath $ B.concat $
                [
                -- wave
                -- magic number
                encInt ("RIFF" :: String)

                -- wave header
                -- data length
                , enc32E (0 :: Int32)
                , encInt ("WAVE" :: String)

                -- format header
                -- magic number
                , encInt ("fmt " :: String)
                -- fmt header size
                , enc32E (16 :: Int32)
                -- +2B = 2B: PCM format
                , enc16E (1 :: Int16)
                -- +2B = 4B: channel count
                , enc16E (chC :: Int16)
                -- +4B = 8B: sample rate
                , enc32E (44100 :: Int32)
                -- +4B = 12B: byte rate
                , enc32E (fromIntegral (44100 * fs) :: Int32)
                -- +2B = 14B: block align
                , enc16E (fromIntegral fs :: Int16)
                -- +2B = 16B: bit per sample
                , enc16E (bitPerSample :: Int16)

                -- data block
                -- magic number
                , encInt ("data" :: String)
                -- data size
                , enc32E (fromIntegral (dataLen - 44) :: Int32)
            ] ++ (map enc16E flatSamples)
        where
            encInt d = B.drop 8 $ encode d
            enc16E d = BB.toLazyByteString $ BB.int16LE (d :: Int16)
            enc32E d = BB.toLazyByteString $ BB.int32LE (d :: Int32)
            fs = chC * (floor (((fromIntegral bitPerSample) + 7) / 8))
            bitPerSample = 16
            dataLen = (length flatSamples) * 2

encInt :: Binary a => a -> B.ByteString
encInt d = B.drop 8 $ encode d

channelToSamples :: RealFrac a => [a] -> [Int16]
channelToSamples samples = map (\s -> floor (remap s (-1.0) 1.0 (fromIntegral (minBound::Int16)) (fromIntegral (maxBound::Int16)))) samples :: [Int16]

render :: Integral t => [Instrument] -> Scores -> Double
    -> [Char] -> TwoChannelMode -> t -> Bool -> Bool -> IO ()
render instruments (Scores bpm bpb dpb trackFx scores) tuning outputPath stereoMode stereoDelay playAfterRender loop = do
    putStrLn "Render mode"
    putStr "Rendering... "

    -- convert input data to internal format
    let track = InternalTrack bpm bpb dpb
            [
                InternalScore sName (map (\n -> toInternalNote n tuning) notes) scoreFx (fromInstrument instrument)
            |
                (Score sName scoreFx notes) <- scores,
                instrument <- instruments,
                sName == (Instrument.name instrument)
            ] trackFx outputPath

    -- get end of track
    let _scores = Render.scores track
    let _allNotes = concat (map Render.notes _scores)
    let _timedNotes = map (\n -> toTimedNote n (bpm) (fromIntegral bpb) (fromIntegral dpb)) _allNotes
    let _endTime = maximum $ map (\(_, _, _, _, _, et) -> et) _timedNotes
    let _sampleCount = round $ 44100 * _endTime :: Int
    let _baseTimes = baseTimes _sampleCount

    -- render individual scores
    let _renderedScores = map (\s -> renderScore s _baseTimes) _scores
    -- sum up all channels
    let _scoreSum = map sum (transpose _renderedScores)
    -- TODO: apply effects
    -- apply sigmoid to prevent clipping
    let _renderedTrack = map sigmoid _scoreSum
    -- generate int samples from doubles
    let _intSamples = trackToSamples stereoMode stereoDelay _renderedTrack

    -- write output file
    renderToFile _intSamples


    if playAfterRender then do
        if (isExt outputPath "wav") then do
            putStrLn "Done"
            putStrLn "Enjoy!"
            play outputPath loop
        else do
            fail "Can't play back raw files but rendering was completed."
    else do
        putStrLn "Done"

    where
        trackToSamples Mono _ samples
                = [channelToSamples samples]
        trackToSamples Invert _ samples
                = [
                    channelToSamples (map (\x -> (-x)) samples),
                    channelToSamples samples
                ]
        trackToSamples Delay sd samples
                = [
                    channelToSamples (n0s ++ samples),
                    channelToSamples (samples ++ n0s)
                ]
                where n0s = [0..(fromIntegral sd)] :: [Double]
        renderToFile samples
                | (isExt outputPath "wav") = writeToWav outputPath (fromIntegral chC) flatSamples
                | otherwise = B.writeFile outputPath (encInt flatSamples)
                where
                    flatSamples = concat $ transpose samples :: [Int16]
                    chC = length samples
        renderScore score baseTimes = do
            let _timedNotes = map (\n -> toTimedNote n (bpm) (fromIntegral bpb) (fromIntegral dpb)) (Render.notes score)
            let _renderedNotes = map (\t -> sum $ map (\n -> intersectingNote t n) _timedNotes) baseTimes

            _renderedNotes
            where intersectingNote time (sF, eF, sV, eV, sT, eT)
                    | time >= sT && time <= eT = do
                            let w = (time - sT) / (eT - sT)
                            let wm = 1 - w
                            let v = w * eV + wm * sV
                            let f = w * eF + wm * sF
                            let t = time - sT
                            let p = t * f
                            atPhase (Render.oscilator score) p v
                    | otherwise = 0
