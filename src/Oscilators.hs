{-# LANGUAGE DeriveGeneric #-}

module Oscilators (
    Oscilator (..),
    fromInstrument,
    atPhase
) where

import GHC.Generics
import Instrument

data Oscilator
    = SilentOsc
    | NoiseOsc
    | SawOsc
    | SineOsc
    | SquareOsc
    | TriangleOsc
    | WavetableOsc {wavetableSamples :: [Double]}
    deriving (Show, Generic)

atPhase :: Oscilator -> Double -> Double -> Double

-- wavetable
atPhase (WavetableOsc samples) phase amplitude = do
    let _samples = (samples) ++ [(samples) !! 0] :: [Double]
    let _phase = (phase - (fromIntegral (floor phase))) :: Double
    let scaled = _phase * (fromIntegral (length $ samples)) :: Double
    let weight = (scaled - (fromIntegral (floor scaled))) :: Double
    let a = _samples !! (floor scaled) :: Double
    let b = _samples !! (ceiling scaled) :: Double
    amplitude * ((1 - weight) * a + weight * b)

atPhase TriangleOsc phase amplitude = amplitude * (sawToTriangle (phase - (fromIntegral (floor phase))))
    where sawToTriangle p
            | p <= 0.25 = p * 4
            | p <= 0.75 = 1 - ((p - 0.25) * (4))
            | otherwise = (p - 0.75) * 4 - 1

atPhase SquareOsc phase amplitude = amplitude * _square (phase - (fromIntegral (floor phase)))
    where _square v
            | v < 0.5 = 1
            | otherwise = -1

atPhase SineOsc phase amplitude = amplitude * (sin (phase * 2 * pi))

atPhase SawOsc phase amplitude = amplitude * ((2 * (phase - (fromIntegral (floor phase))) - 1))

atPhase NoiseOsc phase amplitude = atPhase (WavetableOsc _samples) phase amplitude
    where _samples = [-0.2297973633, -0.1114196777, -0.4096069336, -0.9184265137, 0.9126586914, 0.7923278809, -0.1913757324, -0.9902954102, 0.6774291992, 0.3101196289, 0.8526306152, 0.6728515625, 0.05947875977, 0.551361084, 0.7544250488, 0.3561706543, 0.7624816895, -0.1590881348, -0.4776916504, -0.5687866211, -0.4128417969, 0.6844177246, -0.08288574219, 0.3487854004, 0.3460083008, -0.1226501465, -0.4444274902, 0.8499450684, 0.8678283691, 0.4280700684, -0.4130859375, 0.9141235352, -0.09851074219, -0.06805419922, -0.7156066895, -0.7640075684, 0.3356018066, 0.4515686035, 0.7573242188, 0.3613586426, -0.8981323242, 0.576171875, 0.4049987793, 0.8852539063, 0.7249755859, 0.4207763672, -0.794342041, -0.6380310059, -0.5734558105, -0.3346557617, -0.8497924805, -0.02651977539, 0.9153442383, 0.587097168, 0.5602416992, 0.1820373535, -0.1758728027, 0.8478088379, -0.1572875977, -0.5762023926, 0.1578369141, -0.6814880371, 0.2969055176, -0.8256835938, -0.6711120605, -0.4016113281, 0.2085266113, -0.9132385254, -0.528717041, 0.5514526367, -0.2243652344, 0.4125061035, 0.4862670898, -0.7088317871, 0.1604614258, 0.233581543, -0.2576599121, 0.8085632324, 0.721282959, 0.6790466309, 0.2417907715, 0.1640930176, -0.8157043457, -0.6364746094, -0.4763183594, -0.6940307617, 0.8864135742, -0.601348877, 0.09851074219, -0.8148803711, 0.3463439941, -0.556640625, 0.3970336914, -0.09674072266, 0.1525878906, 0.09265136719, -0.8856506348, 0.5012207031, 0.8792724609, -0.460144043]
    
atPhase SilentOsc phase amplitude = 0

fromInstrument :: Instrument -> Oscilator
fromInstrument (Instrument _ "wavetable" (Just _samples)) = WavetableOsc _samples
fromInstrument (Instrument _ "wavetable" (Nothing)) = error "Samples in wavetable synthesizer cannot be null"
fromInstrument (Instrument _ "triangle" _) = TriangleOsc
fromInstrument (Instrument _ "square" _) = SquareOsc
fromInstrument (Instrument _ "sine" _) = SineOsc
fromInstrument (Instrument _ "saw" _) = SawOsc
fromInstrument (Instrument _ "noise" _) = NoiseOsc
fromInstrument (Instrument _ "silent" _) = SilentOsc
fromInstrument _ = SawOsc -- fallback if no other oscilator matches