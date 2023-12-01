module Aoc
  ( readLines
  , sortDesc
  , splitOn
  , headMay
  , lastMay
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

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay xs = Just $ head xs

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay xs = Just $ last xs
