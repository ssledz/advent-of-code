module Main where

import Prelude hiding ((!))
import Aoc
import Data.List
import Arr (Arr, (!))
import qualified Arr

main :: IO ()
main = do
  run "input/day05.txt"

run :: FilePath -> IO ()
run filePath = do
  putStr "Solution partA: " >> readLines filePath partA
  putStr "Solution partB: " >> readLines filePath partB

type Stacks = Arr [Char]

type Quantity = Int
type From = Int
type To = Int

data Move = M Quantity From To deriving (Show)

readMove :: String -> Move
readMove line = M (read q) (read f) (read t)
  where
    ["move", q, "from", f, "to", t] = words line

readLayer :: String -> String
readLayer = go ""
  where
    f "   "         = " "
    f ['[', c, ']'] = [c]
    go :: String -> String -> String
    go acc [] = acc
    go acc line = let (l, r) = splitAt 3 line
                      rr = if (null r) then r else tail r
                  in go (acc <> f l) rr

readInput :: [String] -> (Stacks, [Move])
readInput lines =
  let (s, c) = break null lines
      ls = readLayer <$> init s
      trim = dropWhile (==' ')
      stacks = trim <$> transpose ls
  in (Arr.fromList stacks, fmap readMove . tail $ c)

partA :: [String] -> String
partA = show . readInput

partB :: [String] -> String
partB = show . length


