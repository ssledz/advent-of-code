module Main where

import Aoc
import qualified Data.Char as Char

main :: IO ()
main = do
  run "input/dayXY.txt"
  -- run "input/day03.txt"

run :: FilePath -> IO ()
run filePath = do
  putStr "Solution partA: " >> readLines filePath partA
  putStr "Solution partB: " >> readLines filePath partB

data Point = Point Int Int deriving (Show, Eq)

data PartNumber = PartNumber Int [Point] deriving Show

data Symbol = Symbol Char Point deriving Show

partNumber :: PartNumber -> Int
partNumber (PartNumber n _) = n

readInput :: [String] -> ([PartNumber], [Symbol])
readInput lines =
  let nlines = zip [0..] lines
  in foldl readRow ([], []) nlines
  where
    readRow (parts, numbers) (row, line) =
      let (_, _, acc) = foldl (readChar row) (Nothing, 0, (parts,numbers)) (line <> ".")
      in acc
    readChar :: Int -> (Maybe (Int, [Point]), Int, ([PartNumber], [Symbol])) -> Char -> (Maybe (Int, [Point]), Int, ([PartNumber], [Symbol]))
    readChar row (Nothing, col, acc@(parts, symbols)) c
        | c == '.'       = (Nothing, col + 1, acc)
        | Char.isDigit c = (Just (read [c], [Point col row]), col + 1, acc)
        | otherwise      = (Nothing, col + 1, (parts, Symbol c (Point col row) : symbols))
    readChar row (Just (n, points), col, acc@(parts, symbols)) c
        | c == '.'       = (Nothing, col + 1, (PartNumber n points : parts, symbols))
        | Char.isDigit c = (Just (read [c] + n * 10, Point col row : points), col + 1, acc)
        | otherwise      = (Nothing, col + 1, (PartNumber n points : parts, Symbol c (Point col row) : symbols))

cartesianP :: [Int] -> [Int] -> [Point]
cartesianP xs ys = do
  x <- xs
  y <- ys
  return $ Point x y

addP :: Point -> Point -> Point
addP (Point x y) (Point x' y') = Point (x + x') (y + y')

adjacents :: Point -> [Point]
adjacents p =
  let disp = cartesianP [-1, 0, 1] [-1, 0, 1]
      positive (Point x y) = x >= 0 && y >= 0
  in filter positive $ map (addP p) disp

partA :: [String] -> String
partA lines =
  let (parts, symbols) = readInput lines
      symbolAdj (Symbol _ p) = adjacents p
      symbolsAdj = symbols >>= symbolAdj
      parts' = filter (\(PartNumber _ points) -> any (\p -> p `elem` points) symbolsAdj) parts
  in show $ sum (map partNumber parts')

partB :: [String] -> String
partB = show . length
