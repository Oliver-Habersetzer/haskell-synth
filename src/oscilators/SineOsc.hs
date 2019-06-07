module SineOsc (
    SineOsc (..)
) where

import Oscilator

data SineOsc = SineOsc {} deriving (Show)

instance Oscilator SineOsc where
    atPhase osc phase amplitude = amplitude * (sin (phase * 2 * pi))