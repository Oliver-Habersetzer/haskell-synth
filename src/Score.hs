{-# LANGUAGE DeriveGeneric #-}

module Score (
    Score,
    readScores
) where

import Key
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BS

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
        end :: String
    } deriving (Show, Generic)

instance FromJSON Note
instance ToJSON Note

data Score = Score
    {
        instrumentName :: String,
        notes :: [Note]
    } deriving (Show, Generic)

instance FromJSON Score
instance ToJSON Score

data Scores = Scores
    {
        beatsPerMinute :: Int,
        beatsPerBar :: Int,
        divisionsPerBeat :: Int,
        scores :: [Score]
    } deriving (Show, Generic)

instance FromJSON Scores
instance ToJSON Scores