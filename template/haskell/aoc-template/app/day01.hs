module Main where

import Aoc

main :: IO ()
main = do
  run "input/dayXY.txt"

run :: FilePath -> IO ()
run filePath = do
  putStr "Solution partA: " >> readLines filePath partA
  putStr "Solution partB: " >> readLines filePath partB

partA :: [String] -> String
partA = show . length

partB :: [String] -> String
partB = show . length
