{-# LANGUAGE DeriveGeneric #-}

module Instrument (
    Instrument,
    readInstruments
) where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BS

readInstruments path = do
    bytes <- BS.readFile path
    let mb = decode bytes :: Maybe (Instruments)
    return $ instruments $ mbV mb
    where mbV (Just a) = a
          mbV (Nothing) = error $ "Could not parse " ++ path ++ " to instruments"


data Instrument = Instrument
    {
        name :: String
    } deriving (Show, Generic)

instance FromJSON Instrument
instance ToJSON Instrument

data Instruments = Instruments
    {
        instruments :: [Instrument]
    } deriving (Show, Generic)

instance FromJSON Instruments
instance ToJSON Instruments