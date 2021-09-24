module Main where

import Data.Char
import Search
import Sudoku
import Problem
import System.Environment

-- replace chars by ints and dots by zeroes
replaceDotsAndChars :: String -> [Int]
replaceDotsAndChars [] = []
replaceDotsAndChars xs = map (\x -> if x == '.' then 0 else digitToInt x) xs

-- reshape list of ints into a 9 x 9 matrix
reshape :: [Int] -> [[Int]]
reshape [] = []
reshape xs = take 9 xs : reshape (drop 9 xs)

main :: IO ()
main = do
  (command : file) <- getArgs

  contents <- readFile $ head file
  let tables = lines contents
  let formatted = map (reshape . replaceDotsAndChars) tables

  let s = Sudoku $ head formatted
  let sols = solve dfs s
  print sols