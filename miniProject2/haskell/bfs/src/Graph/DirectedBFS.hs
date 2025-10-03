{-# OPTIONS_GHC -Wall #-}
module Graph.DirectedBFS
  ( buildAdjacency
  , succOf
  ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS

type Adj = IM.IntMap [Int]

buildAdjacency :: [(Int, Int)] -> Adj
buildAdjacency es =
  let step m (u,v) = IM.insertWith IS.union u (IS.singleton v) m
      msets = foldl' step IM.empty es :: IM.IntMap IS.IntSet
  in IM.map IS.toList msets

succOf :: Adj -> (Int -> [Int])
succOf m x = IM.findWithDefault [] x m