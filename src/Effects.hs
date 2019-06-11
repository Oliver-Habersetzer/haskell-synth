{-# LANGUAGE DeriveGeneric #-}

module Effects (
    Effect (..),
    safeEffectList
) where

import Data.Aeson
import GHC.Generics

safeEffectList :: Maybe [Effect] -> [Effect]
safeEffectList (Just fx) = fx
safeEffectList Nothing = []

data Effect = Effect {
        fxName :: String,
        operators :: [Double]
    } deriving (Show, Generic)
instance FromJSON Effect
instance ToJSON Effect