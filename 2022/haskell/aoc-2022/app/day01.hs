module Main where

import Aoc
import Data.List

main :: IO ()
main = do
  run "input/day01.txt"

run :: FilePath -> IO ()
run filePath = do
  putStr "Solution partA: " >> readLines filePath partA
  putStr "Solution partB: " >> readLines filePath partB

partA :: [String] -> String
partA lines = show $ maxCallories lines

maxCallories :: [String] -> Integer
maxCallories = go 0 0
  where
    go :: Integer -> Integer -> [String] -> Integer
    go maxC currC ("":xs)  = go (max maxC currC) 0 xs
    go maxC currC (x:xs)   = go maxC (currC + read x) xs
    go maxC _ []           = maxC


partB :: [String] -> String
partB = show . sum . take 3 . sortDesc . readCallories

readCallories :: [String] -> [Integer]
readCallories lines = snd $ foldr go (0, []) ("":lines)
  where
    go ""   (acc, cs) = (0, acc:cs)
    go line (acc, cs) = (acc + read line, cs)
