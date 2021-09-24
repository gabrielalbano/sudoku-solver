module Problem where

class Problem a where
  isGoal :: a -> Bool
  expandNodes :: a -> [a]

--initialState :: a