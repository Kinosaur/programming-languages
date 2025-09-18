{-# OPTIONS_GHC -Wall #-}
module Graph.Undirected
  ( buildAdjacency
  , succOf
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.List       (sort)

type Adj = Map.Map Int [Int]

buildAdjacency :: [(Int, Int)] -> Adj
buildAdjacency es =
  let add u v m = Map.insertWith (\new old -> new ++ old) u [v] m
      m1 = foldr (\(u, v) m -> add u v (add v u m)) Map.empty es
  in Map.map (sort . Set.toList . Set.fromList) m1

succOf :: Adj -> (Int -> [Int])
succOf m = \x -> Map.findWithDefault [] x m