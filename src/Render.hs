{-# LANGUAGE ParallelListComp #-}

module Render (
    render
) where
    
import Instrument
import Oscilators
import Score
import Effects
import Data.List
import Data.Int
import qualified Data.ByteString.Lazy as B
import Data.Binary
import AppSettings
import System.IO

clamp x
    | x < -1 = -1
    | x > 1 = 1
    | otherwise = x

sigmoid x = 2.0 / (1.0 + exp (negate x)) - 1.0

remap x in_min in_max out_min out_max = (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min

data InternalTrack = InternalTrack {
        beatsPerMinute :: Int,
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

render instruments (Scores bpm bpb dpb trackFx scores) tuning outputPath stereoMode stereoDelay = do
    putStr "Rendering... "
    hFlush stdout

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
    let _timedNotes = map (\n -> toTimedNote n (fromIntegral bpm) (fromIntegral bpb) (fromIntegral dpb)) _allNotes
    let _endTime = maximum $ map (\(_, _, _, _, _, et) -> et) _timedNotes
    let _sampleCount = round $ 44100 * _endTime :: Int
    let _baseSamples = [0.._sampleCount]
    let _baseTimes = map (\s -> (fromIntegral s) / 44100.0) _baseSamples

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
        channelToSamples samples = map (\s -> floor (remap s (-1.0) 1.0 (fromIntegral (minBound::Int16)) (fromIntegral (maxBound::Int16)))) samples :: [Int16]
        renderToFile samples = B.writeFile outputPath ((encode ((concat $ transpose samples) :: [Int16])))
        renderScore score baseTimes = do
            let _timedNotes = map (\n -> toTimedNote n (fromIntegral bpm) (fromIntegral bpb) (fromIntegral dpb)) (Render.notes score)
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