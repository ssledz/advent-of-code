module Main where

import Aoc
import qualified Data.Char as Char
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

main :: IO ()
main = do
  run "input/day01.txt"

run :: FilePath -> IO ()
run filePath = do
  putStr "Solution partA: " >> readLines filePath partA
  putStr "Solution partB: " >> readLines filePath partB

toDigit :: Char -> Maybe Int
toDigit c | Char.isDigit c = Just $ Char.digitToInt c
          | otherwise = Nothing

digits :: String -> [Int]
digits = mapMaybe toDigit

numbers :: [[Int]] -> [Int]
numbers = map (\xs -> (head xs) * 10 + last xs)

digitsMap :: Map String Int
digitsMap = Map.fromList
  [ ("one", 1)
  , ("two", 2)
  , ("three", 3)
  , ("four", 4)
  , ("five", 5)
  , ("six", 6)
  , ("seven", 7)
  , ("eight", 8)
  , ("nine", 9)
  ]

lookupDigit :: String -> Maybe Int
lookupDigit xs = do
  phrase <- List.find (\x -> x `List.isPrefixOf` xs) (Map.keys digitsMap)
  digit  <- Map.lookup phrase digitsMap
  return digit

toDigit' :: String -> (Maybe Int, String)
toDigit' [] = (Nothing, [])
toDigit' cs@(c:rest) | Char.isDigit c = (toDigit c, rest)
                     | otherwise = (lookupDigit cs, rest)

digits' :: String -> [Int]
digits' = go []
  where
    go acc [] = reverse acc
    go acc xs =
      case toDigit' xs of
        (Just d, rest)  -> go (d : acc) rest
        (Nothing, rest) -> go acc rest

partA :: [String] -> String
partA = show . sum . numbers . map digits

partB :: [String] -> String
partB = show . sum . numbers . map digits'
