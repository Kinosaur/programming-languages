{-# OPTIONS_GHC -Wall #-}
-- DFS path search (first-found under neighbor order).
-- We assume the successor function returns deduped, ascending neighbors.
module Graph.DFS
  ( dfsPath
  ) where

import qualified Data.Set as Set

-- Depth-first search for a path from s to t.
-- Returns the first path found given the neighbor iteration order.
dfsPath :: (Ord a) => (a -> [a]) -> a -> a -> Maybe [a]
dfsPath succF s t = go Set.empty [(s, [s])]
  where
    go _ [] = Nothing
    go seen ((x, path) : rest)
      | x == t = Just path
      | x `Set.member` seen = go seen rest
      | otherwise =
          let seen' = Set.insert x seen
              nbrs = filter (`Set.notMember` seen') (succF x)
              next = [(v, path ++ [v]) | v <- nbrs]
              frontier = next ++ rest
          in go seen' frontier