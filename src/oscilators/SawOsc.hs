module SawOsc (
    SawOsc (..)
) where

import Oscilator

data SawOsc = SawOsc {} deriving (Show)

instance Oscilator SawOsc where
    atPhase osc phase amplitude = amplitude * ((2 * (phase - (fromIntegral (floor phase))) - 1))