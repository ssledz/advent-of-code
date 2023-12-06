module Main where

import Aoc
import Debug.Trace (trace)

main :: IO ()
main = do
  run "input/dayXY.txt"
  run "input/day04.txt"

run :: FilePath -> IO ()
run filePath = do
  putStr "Solution partA: " >> readLines filePath partA
  putStr "Solution partB: " >> readLines filePath partB

data Card = Card Int [Int] [Int] deriving Show

readInput :: [String] -> [Card]
readInput = map readCard

readCard :: String -> Card
readCard line =
  let (card, rest) = splitOn ':' line
      id = read $ drop 5 card
      (win, my) = splitOn '|' rest
      numbers = map read . filter (not . null) . splitOn' ' '
  in Card id (numbers win) (numbers my)


winningNums :: Card -> [Int]
winningNums (Card _ win my) =
  let isWin n = n `elem` win
  in filter isWin my

score :: [Int] -> Int
score xs = sum $ take (length xs) $ 1 : iterate (*2) 1

partA :: [String] -> String
partA = show . sum . map (score . winningNums) . readInput

partB :: [String] -> String
partB = show . length
