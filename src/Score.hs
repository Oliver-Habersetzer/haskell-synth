{-# LANGUAGE DeriveGeneric #-}

module Score (
    Score (Score),
    Scores (Scores),
    Note (Note),
    InternalNote (..),
    readScores,
    writeScore,
    toInternalNote
) where

import Key
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import Effects
import Data.List.Split

writeScore :: FilePath -> Scores -> IO ()
writeScore path scores = BS.writeFile path $ encode scores

readScores :: FilePath -> IO Scores
readScores path = do
    bytes <- BS.readFile path
    let mb = decode bytes :: Maybe (Scores)
    return $ mbV mb
    where mbV (Just a) = a
          mbV (Nothing) = error $ "Could not parse " ++ path ++ " to scores"

data Note = Note
    {
        key :: BaseKey,
        octave :: Int,
        start :: String,
        end :: String,
        vol :: Double,
        -- optional ends for automation
        endKey :: Maybe BaseKey,
        endOctave :: Maybe Int,
        endVol :: Maybe Double
    } deriving (Show, Generic)
instance FromJSON Note
instance ToJSON Note

data InternalNote = InternalNote {
        startFreq :: Double,
        endFreq :: Double,
        startAmp :: Double,
        endAmp :: Double,
        startBar :: Int,
        endBar :: Int,
        startBeat :: Int,
        endBeat :: Int,
        startDivision :: Int,
        endDivision :: Int
    } deriving (Show)

toInternalNote :: Note -> Double -> InternalNote    
toInternalNote a t = do
    let _endKey = justOrDef (endKey a) (key a)
    let _endOctave = justOrDef (endOctave a) (Score.octave a)
    let _start = map read (splitOn "." (start a)) :: [Int]
    let _end = map read (splitOn "." (end a)) :: [Int]
    InternalNote
            (keyToFreq (Key (key a) (Score.octave a)) t)
            (keyToFreq (Key _endKey _endOctave) t)
            (vol a)
            (justOrDef (endVol a) (vol a))
            (_start !! 0)
            (_end !! 0)
            (_start !! 1)
            (_end !! 1)
            (_start !! 2)
            (_end !! 2)
    where
            justOrDef (Just a) _ = a
            justOrDef Nothing def = def
        

data Score = Score
    {
        instrumentName :: String,
        scoreFx :: [Effect],
        notes :: [Note]
    } deriving (Show, Generic)
instance FromJSON Score
instance ToJSON Score

data Scores = Scores
    {
        beatsPerMinute :: Double,
        beatsPerBar :: Int,
        divisionsPerBeat :: Int,
        trackFx :: [Effect],
        scores :: [Score]
    } deriving (Show, Generic)
instance FromJSON Scores
instance ToJSON Scores