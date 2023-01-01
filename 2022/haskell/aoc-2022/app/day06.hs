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

sNcs :: Int -> String -> [String]
sNcs n = go
  where
    go [] = []
    go xs = let h = take n xs
            in h:go (drop 1 xs)

findMarker :: (String -> [String]) -> String -> Maybe (Int, String)
findMarker f = find (unique . snd) . zip [1..] . f

unique :: String -> Bool
unique s = length (nub s) == length s

count :: (Int, String) -> Int
count (c, s) = c + length s - 1

partA :: [String] -> String
partA = show . fmap count . (findMarker $ sNcs 4) . head

partB :: [String] -> String
partB = show . fmap count . (findMarker $ sNcs 14) . head

