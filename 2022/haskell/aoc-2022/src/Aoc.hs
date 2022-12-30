module Aoc
  ( readLines
  , sortDesc
  , splitOn
  ) where

import Data.List

readLines :: FilePath -> ([String] -> String) -> IO ()
readLines fileName f = do
  content <- readFile fileName
  putStrLn $ f (lines content)

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

splitOn :: Char -> String -> (String, String)
splitOn c s = let (l, r) = span (/= c) s
              in (l, tail r)
