{-# LANGUAGE DeriveGeneric #-}

module Key (
    BaseKey (..),
    Key (..)
) where

import GHC.Generics
import Data.Aeson

data Key = Key
    {
        baseKey :: BaseKey,
        octave :: Int
    } deriving (Show, Generic) 

instance FromJSON Key
instance ToJSON Key

data BaseKey
    = A
    | Ais
    | B
    | C
    | Cis
    | D
    | Dis
    | E
    | F
    | Fis
    | G
    | Gis
    deriving (Show, Generic)

instance FromJSON BaseKey
instance ToJSON BaseKey