module SquareOsc (
    SquareOsc (..)
) where

import Oscilator

data SquareOsc = SquareOsc {} deriving (Show)

instance Oscilator SquareOsc where
    atPhase osc phase amplitude = amplitude * _square (phase - (fromIntegral (floor phase)))
        where _square v
                | v < 0.5 = 1
                | otherwise = -1