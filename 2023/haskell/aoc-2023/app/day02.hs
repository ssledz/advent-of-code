module Main where

import Aoc
import Debug.Trace (trace)

main :: IO ()
main = do
  --run "input/dayXX.txt"
  run "input/day02.txt"

run :: FilePath -> IO ()
run filePath = do
  putStr "Solution partA: " >> readLines filePath partA
  putStr "Solution partB: " >> readLines filePath partB


data Game = G Int [Cubes] deriving Show

gid :: Game -> Int
gid (G id _) = id

data Cubes = C { r :: Int, g :: Int, b :: Int} deriving Show

readGame :: String -> Game
readGame line =
  let (game, cubes) = splitOn ':' line
      id = read $ drop 5 game
      cubes' = map (readCubes . triml) $ splitOn' ';' cubes
  in G id cubes'

readCubes :: String -> Cubes
readCubes line =
  let rgbs = map triml $ splitOn' ',' line
  in foldr readRGB (C 0 0 0) rgbs
  where
    readRGB s c =
      let (n, color) = splitOn ' ' s
          n' = read n
      in case color of
        "red"   -> c { r = n'}
        "green" -> c { g = n'}
        "blue"  -> c { b = n'}

maxG :: Game -> Game
maxG (G id cs) =
  let max what = maximum $ map what cs
  in G id [C (max r) (max g) (max b)]


partA :: [String] -> String
partA = show . sum . map gid . filter pred . map (maxG . readGame)
  where
    pred :: Game -> Bool
    pred (G _ [C r' g' b']) = r' <= 12 && g' <= 13 && b' <= 14

partB :: [String] -> String
partB = show . sum . map (power . maxG . readGame)
  where
    power (G _ [C r' g' b']) = r' * g' * b'
