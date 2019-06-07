module TriangleOsc (
    TriangleOsc (..)
) where

import Oscilator

data TriangleOsc = TriangleOsc {} deriving (Show)

instance Oscilator TriangleOsc where
    atPhase osc phase amplitude = amplitude * (sawToTriangle (phase - (fromIntegral (floor phase))))
        where sawToTriangle p
                | p <= 0.25 = p * 4
                | p <= 0.75 = 1 - ((p - 0.25) * (4))
                | otherwise = (p - 0.75) * 4 - 1