module Main where

import Aoc
import Debug.Trace

main :: IO ()
main = do
  run "input/day02.txt"

run :: FilePath -> IO ()
run filePath = do
  putStr "Solution partA: " >> readLines filePath partA
  putStr "Solution partB: " >> readLines filePath partB

partA :: [String] -> String
partA lines = show $ sum $ oneRound <$> lines

data Shape = Rock | Paper | Scissors deriving (Show, Eq)

toShape :: Char -> Shape
toShape 'A' = Rock
toShape 'B' = Paper
toShape 'C' = Scissors
toShape 'X' = Rock
toShape 'Y' = Paper
toShape 'Z' = Scissors

shapeScore :: Shape -> Integer
shapeScore Rock     = 1
shapeScore Paper    = 2
shapeScore Scissors = 3

won :: Shape -> Shape -> Bool
won Rock Paper      = True
won Rock Scissors   = False
won Paper Scissors  = True
won a b | a == b    = False
        | otherwise = not $ won b a

roundOutcome :: Shape -> Shape -> Integer
roundOutcome a b | a == b    = 3
                 | won a b   = 6
                 | otherwise = 0


roundScore :: Shape -> Shape -> Integer
roundScore a b = roundOutcome a b + shapeScore b

oneRound :: String -> Integer
oneRound [a, ' ', b] = roundScore (toShape a) (toShape b)

partB :: [String] -> String
partB lines = show $ length lines
