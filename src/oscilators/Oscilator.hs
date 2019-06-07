module Oscilator (
    Oscilator (..)
) where

class (Show oscilatorType) => Oscilator oscilatorType where
    -- osc, phase, amplitude
    atPhase :: oscilatorType -> Double -> Double -> Double