module Aoc
  ( readLines
  ) where

readLines :: FilePath -> ([String] -> String) -> IO ()
readLines fileName f = do
  content <- readFile fileName
  putStrLn $ f (lines content)
