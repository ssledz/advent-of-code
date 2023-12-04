module Aoc
  ( readLines
  , sortDesc
  , splitOn
  , splitOn'
  , headMay
  , lastMay
  , triml
  ) where

import Data.List
import Data.Maybe

readLines :: FilePath -> ([String] -> String) -> IO ()
readLines fileName f = do
  content <- readFile fileName
  putStrLn $ f (lines content)

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

splitOn :: Char -> String -> (String, String)
splitOn c s = let (l, r) = span (/= c) s
              in (l, fromMaybe [] $ tailMay r)

splitOn' :: Char -> String -> [String]
splitOn' c = go []
  where
    go acc [] = reverse acc
    go acc s  =
      let (l, r) = splitOn c s
      in go (l:acc) r

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay xs = Just $ head xs

tailMay :: [a] -> Maybe [a]
tailMay []  = Nothing
tailMay xs = Just $ tail xs

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay xs = Just $ last xs

triml :: String -> String
triml [] = []
triml s@(c:rest) | c == ' '  = triml rest
                 | otherwise = s
