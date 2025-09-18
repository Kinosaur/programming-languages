{-# OPTIONS_GHC -Wall #-}
module Graph.Directed
  ( buildAdjacency
  , succOf
  ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS

-- Adjacency as a map from node -> ascending list of unique out-neighbors.
type Adj = IM.IntMap [Int]

-- Build adjacency for directed edges: (u,v) means u -> v.
-- Deduplicate neighbors and sort ascending via IntSet.
buildAdjacency :: [(Int, Int)] -> Adj
buildAdjacency es =
  let step m (u,v) = IM.insertWith IS.union u (IS.singleton v) m
      msets = foldl step IM.empty es :: IM.IntMap IS.IntSet
  in IM.map IS.toList msets

succOf :: Adj -> (Int -> [Int])
succOf m = \x -> IM.findWithDefault [] x m