{-# LANGUAGE ParallelListComp #-}

module Render (
    render
) where
    
import Instrument
import Oscilators
import Score
import Effects

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

render instruments (Scores bpm bpb dpb trackFx scores) tuning outputPath = do
    let asdf = InternalTrack bpm bpb dpb 
            [
                InternalScore sName (map (\n -> toInternalNote n tuning) notes) scoreFx (fromInstrument instrument)
            |
                (Score sName scoreFx notes) <- scores,
                instrument <- instruments,
                sName == (Instrument.name instrument)
            ] trackFx outputPath
    putStrLn $ show asdf
    putStrLn "asdf"