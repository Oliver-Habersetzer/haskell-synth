module Utils (
    isExt,
    isExts
) where

import Data.Char
import Data.List.Split

isExt path ext = (map toLower (last (splitOn "." path))) == (map toLower ext)
isExts path exts = or $ map (\ext -> isExt path ext) exts