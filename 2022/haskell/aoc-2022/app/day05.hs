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
  in (Arr.elems stacks, fmap readMove . tail $ c)


doMove :: Move -> Stacks -> Stacks
doMove (M 0 _ _) stacks          = stacks
doMove (M _ i j) stacks | i == j = stacks
doMove (M q i j) stacks =
  let from = stacks ! (i - 1)
      update = Arr.adjust (i - 1) tail . Arr.adjust (j - 1) (head from :)
  in doMove (M (q - 1) i j) (update stacks)

partA :: [String] -> String
partA = show . go . readInput
  where
    go (stacks, moves) =
      let stacks' = foldl' (flip doMove) stacks moves
      in Arr.toList $ Arr.map head stacks'
    go' (stacks, moves) =
      let update = foldr (.) id $ fmap doMove (reverse moves)
          stacks' = update stacks
      in Arr.toList $ Arr.map head stacks'

partB :: [String] -> String
partB = show . length


