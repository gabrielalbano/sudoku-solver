module Sudoku where

import Data.List
import Problem
import Debug.Trace

--newtype Sudoku a = Sudoku [[a]] deriving (Eq, Ord, Show)
newtype Sudoku = Sudoku (Matrix Int) deriving (Eq, Ord, Show)

--type Sudoku = Matrix Value
type Matrix a = [Row a]

type Row a = [a]

type Choices = [Value]

type Value = Int

instance Problem Sudoku where
  --isGoal (Sudoku s) = sumEachRowEquals45 (Sudoku s) && sumEachColumnEquals45 (Sudoku (transpose s)) && sumEachQuadrantEquals45 (Sudoku s)
  isGoal (Sudoku s) = sumEachRowEquals45 (Sudoku s)
                   && sumEachColumnEquals45 (Sudoku s)
                   && sumEachQuadrantEquals45 (Sudoku  s)

  expandNodes s = filter (isValid noDuplicates) [replaceNextEmptySlot s n | hasEmptySlot s, n <- [1 .. 9]]

hasEmptySlot :: Sudoku -> Bool
hasEmptySlot (Sudoku s) = any (0 `elem`) s

replaceNextEmptySlot :: Sudoku -> Int -> Sudoku
replaceNextEmptySlot (Sudoku s) n = Sudoku newSudoku
  where
    newSudoku = sudokuHead ++ (rowHead ++ n:rowTail):sudokuTail
    (sudokuHead, row:sudokuTail) = break (0 `elem`) s
    (rowHead, empty:rowTail) = break (0 ==) row

isValid :: ([Int] -> Bool) -> Sudoku  -> Bool
-- isValid (Sudoku s) = (trace $ show s) all noDuplicates s && all noDuplicates (transpose s) && all noDuplicates (matrixToQuadrant s)
-- isValid f (Sudoku s) = (trace $ show s) all f s && all f (transpose s) && all f (matrixToQuadrant s)
isValid f (Sudoku s) = all f s && all f (transpose s) && all f (matrixToQuadrant s)

noDuplicatesGoal :: (Eq a, Num a) => Row a -> Bool
noDuplicatesGoal [] = True
noDuplicatesGoal (x:xs) = x `notElem` xs && noDuplicatesGoal xs

noDuplicates :: (Eq a, Num a) => Row a -> Bool
noDuplicates [] = True
noDuplicates (x:xs) = if x /= 0 then x `notElem` xs && noDuplicates xs else noDuplicates xs

sumEachRowEquals45 :: Sudoku -> Bool
--sumEachRowEquals45 (Sudoku s) = let sumRows = map sum s in trace ("rows " ++ show (map sum s)) all (== 45) sumRows
sumEachRowEquals45 (Sudoku s) = let sumRows = map sum s in all (== 45) sumRows

sumEachColumnEquals45 :: Sudoku -> Bool
--sumEachColumnEquals45 (Sudoku s) = let sumColumns = map sum s in trace ("columns " ++ show (map sum s)) all (== 45) sumColumns
sumEachColumnEquals45 (Sudoku s) = let sumColumns = map sum (transpose s) in all (== 45) sumColumns

sumEachQuadrantEquals45 :: Sudoku -> Bool
sumEachQuadrantEquals45 (Sudoku s) = sumEachRowEquals45 (Sudoku (matrixToQuadrant s))

matrixToQuadrant :: Matrix a -> Matrix a
matrixToQuadrant = unpack . map cols . pack
  where
    pack = split . map split
    split = chop 3
    unpack = map concat . concat
    cols s = transpose s

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)