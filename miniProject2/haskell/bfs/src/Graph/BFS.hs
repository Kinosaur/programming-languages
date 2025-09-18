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

-- Returns the shortest path (by edges) from s to t under the given neighbor order,
-- or Nothing if no path exists.
bfsPath :: (Int -> [Int]) -> Int -> Int -> Maybe [Int]
bfsPath succF s t
  | s == t    = Just [s]
  | otherwise = loop initialVisited initialParents (Seq.singleton s)
  where
    initialVisited = IS.singleton s
    initialParents = IM.empty :: IM.IntMap Int

    loop :: IS.IntSet -> IM.IntMap Int -> Seq.Seq Int -> Maybe [Int]
    loop visited parents queue = case Seq.viewl queue of
      Seq.EmptyL -> Nothing
      u Seq.:< rest ->
        if u == t
          then Just (reconstruct parents s t)
          else
            let nbrs = succF u
                (visited', parents', rest') =
                  foldl
                    (\(vi, pa, q) v ->
                       if IS.member v vi
                         then (vi, pa, q)
                         else ( IS.insert v vi
                              , IM.insert v u pa
                              , q Seq.|> v
                              )
                    )
                    (visited, parents, rest)
                    nbrs
            in loop visited' parents' rest'

    reconstruct :: IM.IntMap Int -> Int -> Int -> [Int]
    reconstruct parents s0 t0 = reverse (go t0)
      where
        go cur
          | cur == s0 = [s0]
          | otherwise = case IM.lookup cur parents of
                          Nothing -> [s0]     -- should not happen if BFS found t
                          Just p  -> cur : go p