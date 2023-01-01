module Main where

import Aoc
import Data.List

main :: IO ()
main = do
  run "input/day06.txt"

run :: FilePath -> IO ()
run filePath = do
  putStr "Solution partA: " >> readLines filePath partA
  putStr "Solution partB: " >> readLines filePath partB

fcs :: String -> [String]
fcs line =
  let f xs c = xs <> [c]
      g a b  = a:b:[]
      s1 = zipWith g line (drop 1 line)
      s2 = zipWith f s1 (drop 2 line)
  in  zipWith f s2 (drop 3 line)

findMarker :: String -> Maybe (Int, String)
findMarker = find p . zip [1..] . fcs
  where
    p (_, s) = length (nub s) == length s

partA :: [String] -> String
partA = show . fmap ((+3) . fst) . findMarker . head

partB :: [String] -> String
partB = show . length
