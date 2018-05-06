module FileHandle where

import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
  inh <- openFile "input.txt" ReadMode
  outh <- openFile "output.txt" WriteMode
  toUpperFile inh outh
  hClose inh
  hClose outh

toUpperFile :: Handle -> Handle -> IO ()
toUpperFile inh outh = do
  ineof <- hIsEOF inh
  if ineof
    then return ()
    else
    do
      line <- hGetLine inh
      hPutStrLn outh (map toUpper line)
      toUpperFile inh outh
