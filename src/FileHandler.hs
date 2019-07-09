module FileHandler (
  saveNote,
  deleteNote
) where

import System.IO
import System.Directory
import Control.Concurrent
import Data.List.Split
import Key

--saveNote :: Key -> IO ()
saveNote key = do
  file <- openFile ".tmp" AppendMode
  hPutStrLn file $ remstr $ show key
  hClose file
  --forkIO $ test key
  return ()

--deleteNote :: [Char] -> IO ()
deleteNote key = do
  file <- openFile ".tmp" ReadMode
  input <- loop file key []
  let split = splitOn "/" input
  hClose file
  writeFile ".tmp" ""
  file <- openFile ".tmp" WriteMode
  mapM_ (hPutStrLn file) split
  hClose file
  return ()


--loop :: Handle -> Key -> [Char] ->  IO ([Char])
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

--remstr :: [Char] -> [Char]
remstr [] = []
remstr (x:xs) = if x == '"' then remstr xs else x:remstr xs

test :: [Char] -> IO ()
test key = do
  print $ "Ich bin geforked von " ++ key
  file <- openFile ".tmp" ReadMode
  content <- hGetContents file
  if(elem key $ splitOn "\n" $ remstr content)
    then
      do
        threadDelay 10000000
        test key
    else
      do
        threadID <- myThreadId
        hClose file
        killThread threadID
