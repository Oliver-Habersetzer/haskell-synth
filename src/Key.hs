{-# LANGUAGE DeriveGeneric #-}

module Key (
    BaseKey (..),
    Key (..),
    keyToFreq,
    qwertzToKey,
    qwertzToFreq,
    midiPitchToBaseKey,
    baseKeys,
    midiPitchToOctave,
    keyPath,
    baseKeyIndex
) where

import GHC.Generics
import Data.Aeson
import Data.Char

data Key = NoKey | Key
    {
        baseKey :: BaseKey,
        octave :: Int
    } deriving (Show, Generic) 
instance FromJSON Key
instance ToJSON Key

data BaseKey = A | Ais | B | C | Cis | D | Dis | E | F | Fis | G | Gis deriving (Show, Generic)
instance FromJSON BaseKey
instance ToJSON BaseKey

baseKeys :: [BaseKey]
baseKeys = [C, Cis, D, Dis, E, F, Fis, G, Gis, A, Ais, B]

baseKeyIndex :: BaseKey -> Double
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

keyPath :: Key -> String
keyPath (Key bk oct) = "./samples/" ++ (show $ bk) ++ (show $ oct) ++ ".wav"

midiPitchToOctave :: Integral a => a -> a
midiPitchToOctave midiPitch = quot (midiPitch - 12) 12

midiPitchToBaseKey :: Integral a => a -> BaseKey
midiPitchToBaseKey midiPitch = _midiPitchToBaseKey $ midiPitch `mod` 12
        where
                _midiPitchToBaseKey 9 = A
                _midiPitchToBaseKey 10 = Ais
                _midiPitchToBaseKey 11 = B
                _midiPitchToBaseKey 0 = C
                _midiPitchToBaseKey 1 = Cis
                _midiPitchToBaseKey 2 = D
                _midiPitchToBaseKey 3 = Dis
                _midiPitchToBaseKey 4 = E
                _midiPitchToBaseKey 5 = F
                _midiPitchToBaseKey 6 = Fis
                _midiPitchToBaseKey 7 = G
                _midiPitchToBaseKey 8 = Gis

keyToFreq :: Key -> Double -> Double
keyToFreq (Key baseKey octave) tuning = transpose (baseKeyFreq baseKey tuning) (octave - 4)

baseKeyFreq :: BaseKey -> Double -> Double
baseKeyFreq baseKey tuning = tuning * (2.0 ** ((baseKeyIndex baseKey) / 12.0))

transpose :: Double -> Int -> Double
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
            | 'i' == c = Key C (6 + o)
            | '9' == c = Key Cis (6 + o)
            | 'o' == c = Key D (6 + o)
            | '0' == c = Key Dis (6 + o)
            | 'p' == c = Key E (6 + o)
            | otherwise = NoKey

qwertzToFreq :: Char -> Int -> Double -> Double
qwertzToFreq char offset tuning = keyToFreq (qwertzToKey char offset) tuning