module Utils (
    isExt,
    isExts
) where

import Data.Char
import Data.List.Split

isExt :: [Char] -> [Char] -> Bool
isExt path ext = (map toLower (last (splitOn "." path))) == (map toLower ext)

isExts :: [Char] -> [[Char]] -> Bool
isExts path exts = or $ map (\ext -> isExt path ext) exts
