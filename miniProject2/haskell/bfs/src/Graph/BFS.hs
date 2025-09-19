{-# OPTIONS_GHC -Wall #-}
-- Queue-based BFS path search:
-- - Mark visited when enqueuing neighbors.
-- - Stop when the target node is dequeued.
-- - Reconstruct path via a parent map.
module Graph.BFS
  ( bfsPath
  ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS
import qualified Data.Sequence      as Seq
import           Data.Sequence (Seq, ViewL(..), (|>))

-- Returns the shortest path (by edges) from s to t under the given neighbor order,
-- or Nothing if no path exists.
bfsPath :: (Int -> [Int]) -> Int -> Int -> Maybe [Int]
bfsPath succF s t
  | s == t    = Just [s]
  | otherwise = loop initialVisited initialParents (Seq.singleton s)
  where
    initialVisited = IS.singleton s
    initialParents = IM.empty :: IM.IntMap Int

    loop :: IS.IntSet -> IM.IntMap Int -> Seq Int -> Maybe [Int]
    loop visited parents queue =
      case Seq.viewl queue of
        EmptyL    -> Nothing
        u :< rest ->
          if u == t
            then Just (reconstruct parents s t)
            else
              let step (vi, pa, q) v
                    | IS.member v vi = (vi, pa, q)
                    | otherwise      = ( IS.insert v vi
                                        , IM.insert v u pa
                                        , q |> v
                                        )
                  (visited', parents', rest') =
                    foldl' step (visited, parents, rest) (succF u)
              in loop visited' parents' rest'

    reconstruct :: IM.IntMap Int -> Int -> Int -> [Int]
    reconstruct parents s0 t0 = reverse (go t0)
      where
        go cur
          | cur == s0 = [s0]
          | otherwise = case IM.lookup cur parents of
                          Just p  -> cur : go p
                          Nothing -> error "BFS: inconsistent parent map during reconstruction"