module Main where

import Aoc
import Data.List

main :: IO ()
main = do
  run "input/day04.txt"

run :: FilePath -> IO ()
run filePath = do
  putStr "Solution partA: " >> readLines filePath partA
  putStr "Solution partB: " >> readLines filePath partB

partA :: [String] -> String
partA = show . length . filter (uncurry contains) . fmap readRangeTuple
  where
    contains r l = r `containsR` l || l `containsR` r

data Range = R Int Int deriving (Show)

containsR :: Range -> Range -> Bool
containsR (R l1 l2) (R r1 r2) = l1 <= r1 && l2 >= r2

readRange :: String -> Range
readRange line =
  let (l, r) = splitOn '-' line
  in R (read l) (read r)

readRangeTuple :: String -> (Range, Range)
readRangeTuple line =
  let (l, r) = splitOn ',' line
  in (readRange l, readRange r)

splitOn :: Char -> String -> (String, String)
splitOn c s = let (l, r) = span (/= c) s
              in (l, tail r)

partB :: [String] -> String
partB lines = ""
