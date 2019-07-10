module FileHandler (
  saveNote,
  deleteNote,
  getTMP
) where

import System.IO
import Control.Concurrent
import Data.List.Split
import Key
import Data.Typeable

saveNote :: [Char] -> IO (Bool)
saveNote key = do
  bool <- checkNote key
  if  (not bool)
    then
      do
        file <- openFile "./.hs-synth-tmp" AppendMode
        hPutStrLn file $ remstr $ show key
        hClose file
        return (True)
    else
      return (False)

deleteNote :: [Char] -> IO ()
deleteNote key = do
  file <- openFile "./.hs-synth-tmp" ReadMode
  input <- loop file key []
  let split = splitOn "/" input
  hClose file
  writeFile "./.hs-synth-tmp" ""
  file <- openFile "./.hs-synth-tmp" WriteMode
  mapM_ (hPutStrLn file) (filter (\x -> not (x == "")) split)
  hClose file
  return ()


loop :: Handle -> [Char] -> [Char] ->  IO ([Char])
loop file key list = do
  bool <- hIsEOF file
  if bool
    then return (list)
    else
      do
        input <- hGetLine file
        let removed = remstr input
        if removed == key
          then loop file key list
          else loop file key ( list ++ removed ++ "/")

checkFile :: Handle -> [Char] -> IO (Bool)
checkFile file key =
  do
  bool <- hIsEOF file
  if bool
    then return False
    else
      do
        input <- hGetLine file
        let removed = remstr input
        if removed == key
          then return True
          else checkFile file key


remstr :: [Char] -> [Char]
remstr [] = []
remstr (x:xs) = if x == '"' then remstr xs else x:remstr xs

checkNote key = do
  file <- openFile "./.hs-synth-tmp" ReadMode
  bool <- checkFile file key
  hClose file
  if (bool)
    then return True
    else return False

getTMP :: IO [String]
getTMP = do
  file <- openFile "./.hs-synth-tmp" ReadMode
  list <- getSamples file []
  hClose file
  return (list)

getSamples :: Handle -> [String] -> IO ([String])
getSamples file list = do
  bool <- hIsEOF file
  if bool
    then return (list)
    else do
      nextEle <- hGetLine file
      getSamples file $ nextEle:list
