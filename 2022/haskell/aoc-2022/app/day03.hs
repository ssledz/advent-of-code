module Main where

import Aoc
import Data.Char
import Data.List

main :: IO ()
main = do
  run "input/day03.txt"

run :: FilePath -> IO ()
run filePath = do
  putStr "Solution partA: " >> readLines filePath partA
  putStr "Solution partB: " >> readLines filePath partB

partA :: [String] -> String
partA = show . sum . fmap priority . allCommonItems

allCommonItems :: [String] -> String
allCommonItems = foldl f ""
  where
    f acc = (acc <>) . commonItems . toCompartments

priority :: Char -> Int
priority c | isLower c = 1  - fromEnum 'a' + fromEnum c
           | otherwise = 27 -  fromEnum 'A' + fromEnum c

toCompartments :: String -> (String, String)
toCompartments s = splitAt idx s
  where
    idx = round $ fromIntegral (length s) / 2

commonItems :: (String, String) -> String
commonItems (c1, c2) = filter f (nub c1)
  where
    f c = elem c c2

partB :: [String] -> String
partB = show . sum . fmap priority . go
  where
    go = (foldMap commonItemsInGroup) . toGroups


toGroups :: [String] -> [(String, String, String)]
toGroups = go []
  where
    go acc lines | length lines < 3 = acc
    go acc lines = let ([a, b, c], other) = splitAt 3 lines
                   in go ((a, b, c) : acc) other

commonItemsInGroup :: (String, String, String) -> String
commonItemsInGroup (a, b, c) = commonItems (commonItems (a, b), c)
