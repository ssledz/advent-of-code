module Main where

import Aoc
import Data.Char as Char
import Data.Maybe

main :: IO ()
main = do
  run "input/day01.txt"
  -- run "input/dayXX.txt"

run :: FilePath -> IO ()
run filePath = do
  putStr "Solution partA: " >> readLines filePath partA
  putStr "Solution partB: " >> readLines filePath partB

toDigit :: Char -> Maybe Int
toDigit c | Char.isDigit c = Just $ Char.digitToInt c
          | otherwise = Nothing

digits :: [String] -> [[Int]]
digits lines = map (mapMaybe toDigit) lines

numbers :: [[Int]] -> [Int]
numbers = map (\xs -> (head xs) * 10 + last xs)

partA :: [String] -> String
partA = show . sum . numbers . digits
-- partA = show  . digits

partB :: [String] -> String
partB = show . length
