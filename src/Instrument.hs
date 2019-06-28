{-# LANGUAGE DeriveGeneric #-}

module Instrument (
    Instrument (..),
    readInstruments
) where

import Data.Aeson
import GHC.Generics
import System.Directory
import qualified Data.ByteString.Lazy as BS

readInstruments :: FilePath -> IO [Instrument]
readInstruments path = do
    if path == "" then do
        return baseInstruments
    else do
        isValidPath <- doesFileExist path
        if isValidPath then do 
            bytes <- BS.readFile path
            let mb = decode bytes :: Maybe (Instruments)
            return $ baseInstruments ++ (instruments $ mbV mb)
        else
            error $ "The file " ++ path ++ " does not exist"
    where mbV (Just a) = a
          mbV (Nothing) = error $ "Could not parse " ++ path ++ " to instruments"

baseInstruments = [bo si, bo sa, bo sq, bo tr]
    where
        bo osc = Instrument osc osc Nothing
        si = "sine"
        sa = "saw"
        sq = "square"
        tr = "triangle"

data Instrument = Instrument
    {
        name :: String,
        oscilator :: String,
        samples :: Maybe [Double]
    } deriving (Show, Generic)

instance FromJSON Instrument
instance ToJSON Instrument

data Instruments = Instruments
    {
        instruments :: [Instrument]
    } deriving (Show, Generic)

instance FromJSON Instruments
instance ToJSON Instruments