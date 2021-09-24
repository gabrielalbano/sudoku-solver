module Search where

import Data.List (sortOn)
import Data.Set hiding (filter)
import Problem
import Sudoku
import Debug.Trace

data Solution a = Fail | Interrupted | Incomplete | Solved a deriving (Show)

type Queue a = [a]

dfs, bfs :: (Problem a, Show a) => Queue a -> Queue a
dfs [] = []
dfs (q : qs) = expandNodes q ++ qs
bfs [] = []
bfs (q : qs) = qs ++ expandNodes q

treeSearchSteps :: (Show a, Ord a, Problem a) => (Queue a -> Queue a) -> a -> [a]
--treeSearchSteps step initialState = trace ("initial state: " ++ show initialState) initialState : go [initialState] empty
treeSearchSteps step initialState = initialState : go [initialState] empty
  where
    go [] visited = []
    go queue@(q : _) visited
      | isGoal q = [q]
      | otherwise = q : go queue' (insert q visited)
      where
        queue' = filter (`notMember` visited) $ step queue

treeSearch :: (Show a, Ord a, Problem a) => (Queue a -> Queue a) -> a -> Solution a
treeSearch inserter initialState = go [initialState] empty
  where
    go [] visited = Fail
    go queue@(q : _) visited =
      if isGoal q
        then Solved q
        else go queue' (insert q visited)
      where
        queue' = filter (`notMember` visited) (inserter queue)

solve :: (Show a, Ord a, Problem a) => (Queue a -> Queue a) -> a -> [a]
--solve :: (Show a, Ord a, Problem a) => a -> (Queue a -> Queue a) -> Solution a
solve = treeSearchSteps

-- main :: IO ()
-- main = do
--   --let sols = (treeSearchSteps (astar heurPuzzle2) :: [Puzzle])
--   let sols = (treeSearchSteps bfs :: [Queen])
--   -- mapM_ print (treeSearchSteps (astar heurGraph) :: [Path])
--   --  puzzleAnim sols
--   queenAnim sols

-- --graphAnim sols