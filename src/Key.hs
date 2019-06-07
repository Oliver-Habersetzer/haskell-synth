{-# LANGUAGE DeriveGeneric #-}

module Key (
    BaseKey (..),
    Key (..),
    keyToFreq,
    qwertzToKey,
    qwertzToFreq
) where

import GHC.Generics
import Data.Aeson
import Data.Char

data Key = Key
    {
        baseKey :: BaseKey,
        octave :: Int
    } deriving (Show, Generic) 
instance FromJSON Key
instance ToJSON Key

data BaseKey = A | Ais | B | C | Cis | D | Dis | E | F | Fis | G | Gis deriving (Show, Generic)
instance FromJSON BaseKey
instance ToJSON BaseKey

baseKeyIndex :: BaseKey -> Float
baseKeyIndex A = 0
baseKeyIndex Ais = 1
baseKeyIndex B  = 2
baseKeyIndex C = -9
baseKeyIndex Cis = -8
baseKeyIndex D = -7
baseKeyIndex Dis = -6
baseKeyIndex E = -5
baseKeyIndex F = -4
baseKeyIndex Fis = -3
baseKeyIndex G = -2
baseKeyIndex Gis = -1

keyToFreq :: Key -> Float -> Float
keyToFreq (Key baseKey octave) tuning = transpose (baseKeyFreq baseKey tuning) (octave - 4)

baseKeyFreq :: BaseKey -> Float -> Float
baseKeyFreq baseKey tuning = tuning * (2.0 ** ((baseKeyIndex baseKey) / 12.0))

transpose :: Float -> Int -> Float
transpose baseFreq octave
        | octave < 0
            = transpose (baseFreq / 2.0) (octave + 1)
        | octave > 0
            = transpose (baseFreq * 2.0) (octave - 1)
        | otherwise
            = baseFreq

qwertzToKey :: Char -> Int -> Key
qwertzToKey c o = _int $ toLower c
    where _int c
            -- oct 4 + o
            | 'y' == c = Key C (4 + o)
            | 's' == c = Key Cis (4 + o)
            | 'x' == c = Key D (4 + o)
            | 'd' == c = Key Dis (4 + o)
            | 'c' == c = Key E (4 + o)
            | 'v' == c = Key F (4 + o)
            | 'g' == c = Key Fis (4 + o)
            | 'b' == c = Key G (4 + o)
            | 'h' == c = Key Gis (4 + o)
            | 'n' == c = Key A (4 + o)
            | 'j' == c = Key Ais (4 + o)
            | 'm' == c = Key B (4 + o)
            -- oct 5 + o
            | elem c [',', 'q'] = Key C (5 + o)
            | elem c ['l', '2'] = Key Cis (5 + o)
            | elem c ['.', 'w'] = Key D (5 + o)
            | elem c ['ö', '3'] = Key Dis (5 + o)
            | elem c ['-', 'e'] = Key E (5 + o)
            | 'r' == c = Key F (5 + o)
            | '5' == c = Key Fis (5 + o)
            | 't' == c = Key G (5 + o)
            | '6' == c = Key Gis (5 + o)
            | 'z' == c = Key A (5 + o)
            | '7' == c = Key Ais (5 + o)
            | 'u' == c = Key B (5 + o)
            -- oct 6 + o
            | 'i' == c = Key C (4 + o)
            | '9' == c = Key Cis (4 + o)
            | 'o' == c = Key D (4 + o)
            | '0' == c = Key Dis (4 + o)
            | 'p' == c = Key E (4 + o)

qwertzToFreq :: Char -> Int -> Float -> Float
qwertzToFreq char offset tuning = keyToFreq (qwertzToKey char offset) tuning