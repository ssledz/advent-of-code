module Aoc
  ( readLines
  , sortDesc
  ) where

import Data.List

readLines :: FilePath -> ([String] -> String) -> IO ()
readLines fileName f = do
  content <- readFile fileName
  putStrLn $ f (lines content)

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)
