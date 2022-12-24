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
  where
    oneRound [a, ' ', b] = roundScore (toShape a) (toShape b)

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

partB :: [String] -> String
partB lines = show $ sum $ oneRound <$> lines
  where
    oneRound [a, ' ', b] =
      let shape1 = toShape a
      in roundScore shape1 (toShape2 shape1 b)

toShape2 :: Shape -> Char -> Shape
toShape2 Rock     'X' = Scissors
toShape2 Scissors 'X' = Paper
toShape2 Paper    'X' = Rock
toShape2 a        'Y' = a
toShape2 Rock     'Z' = Paper
toShape2 Scissors 'Z' = Rock
toShape2 Paper    'Z' = Scissors
