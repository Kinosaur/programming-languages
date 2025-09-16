{-# OPTIONS_GHC -Wall #-}

-- Depth-first search (DFS), with:
-- - Directed and undirected graph builders
-- - Deterministic ordering: smallest neighbor first (per expansion)
-- - Concise step trace to understand the algorithm
--
-- Key mechanics:
-- - frontier is a stack (list head = top). We "push" successors in front.
-- - seen prevents revisiting nodes (cycle-safe).
-- - Sorting successors makes DFS choose the smallest neighbor first locally.
module Graph.DFS
    ( dfs,
        dfsTrace,
        fromDirectedEdges,
        fromUndirectedEdges,
        adjacencyOf,
        exampleDirectedEdges,
        exampleUndirectedEdges,
    ) where

import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Pure DFS traversal (pre-order).
-- Visits the smallest successor first by sorting neighbors at each expansion.
dfs :: (Ord a) => (a -> [a]) -> a -> [a]
dfs succF start = go Set.empty [start]
    where
    go _ [] = []
    go seen (x : xs)
        | x `Set.member` seen = go seen xs
        | otherwise =
            let seen' = Set.insert x seen
                successors = sort (succF x) -- ensures smallest neighbor first (locally)
                frontier' = successors ++ xs -- LIFO stack: push in front
            in x : go seen' frontier'

-- Concise, human-readable step trace (minimal but useful):
-- - "pop n -> visit; push [..]" when n is first seen
-- - "pop n -> skip (seen)" when n was already visited
dfsTrace :: (Ord a, Show a) => (a -> [a]) -> a -> [String]
dfsTrace succF start = go Set.empty [start]
    where
    go _ [] = ["frontier empty -> stop"]
    go seen (x : xs)
        | x `Set.member` seen =
            ("pop " <> show x <> " -> skip (seen)") : go seen xs
        | otherwise =
            let successors = sort (succF x)
                seen' = Set.insert x seen
                frontier' = successors ++ xs
            in ("pop " <> show x <> " -> visit")
                : ("push " <> show successors <> " (successors of " <> show x <> ")")
                : go seen' frontier'

-- Directed edges builder: (u -> v).
fromDirectedEdges :: (Ord a) => [(a, a)] -> Map a [a]
fromDirectedEdges es =
    Map.fromListWith (++) [(u, [v]) | (u, v) <- es]

-- Undirected edges builder: (u -- v) becomes u->v and v->u.
fromUndirectedEdges :: (Ord a) => [(a, a)] -> Map a [a]
fromUndirectedEdges es =
    Map.fromListWith (++) (concat [[(u, [v]), (v, [u])] | (u, v) <- es, u /= v])

-- Turn an adjacency map into a successor function.
adjacencyOf :: (Ord a) => Map a [a] -> (a -> [a])
adjacencyOf m x = Map.findWithDefault [] x m

-- Example graphs
-- Directed: includes a cycle 3 -> 1 to show "skip (seen)" lines.
exampleDirectedEdges :: [(Int, Int)]
exampleDirectedEdges =
    [(1, 2), (1, 4), (2, 3), (2, 5), (3, 1)]

-- Undirected counterpart (no explicit cycle edge; undirected implies both directions).
exampleUndirectedEdges :: [(Int, Int)]
exampleUndirectedEdges =
    [(1, 2), (1, 4), (2, 3), (2, 5)]