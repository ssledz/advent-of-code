module Main where

import Aoc

main :: IO ()
main = do
  run "input/day07.txt"

run :: FilePath -> IO ()
run filePath = do
  putStr "Solution partA: " >> readLines filePath partA
  putStr "Solution partB: " >> readLines filePath partB

data INode = Dir String [INode] | File Int String deriving (Show)

data Command = CD String | LS [INode] deriving (Show)

readInode :: String -> INode
readInode line =
  let (l, name) = splitOn ' ' line
      dir = Dir name []
      file = File (read l) name
  in if (l == "dir") then dir else file

readInput :: [String] -> [Command]
readInput = reverse . go ([], [])
  where
    go (cmds, nodes) lines@(line:rest)
      | head line == '$' && not (null nodes) = go (LS nodes : cmds, []) lines
      | take 4 line == "$ cd" = go (CD (drop 5 line) : cmds, []) rest
      | take 4 line == "$ ls" = go (cmds, []) rest
      | otherwise = go (cmds, readInode line : nodes) rest
    go (cmds, []) [] = cmds
    go (cmds, nodes) [] = LS nodes : cmds

partA :: [String] -> String
partA = show . readInput

partB :: [String] -> String
partB = show . length
