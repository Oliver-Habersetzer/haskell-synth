module WavetableOsc (
    WavetableOsc (..)
) where

import Oscilator

data WavetableOsc = WavetableOsc
    {
        samples :: [Double]
    } deriving (Show)

instance Oscilator WavetableOsc where
    atPhase osc phase amplitude = do
        let _samples = (samples osc) ++ [(samples osc) !! 0] :: [Double]
        let _phase = (phase - (fromIntegral (floor phase))) :: Double
        let scaled = _phase * (fromIntegral (length $ samples osc)) :: Double
        let weight = (scaled - (fromIntegral (floor scaled))) :: Double
        let a = _samples !! (floor scaled) :: Double
        let b = _samples !! (ceiling scaled) :: Double
        amplitude * ((1 - weight) * a + weight * b)