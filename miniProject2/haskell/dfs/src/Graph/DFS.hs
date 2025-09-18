{-# OPTIONS_GHC -Wall #-}
-- Depth-first search for undirected graphs (works for any successor function).
-- Deterministic choice: smallest neighbor first (per expansion) is achieved
-- by providing a successor function whose neighbor lists are already sorted.
-- We assume succF returns deduplicated, sorted neighbors (our Undirected builder ensures this).
--
-- Model:
--   - 'seen' is a persistent Set of visited nodes to prevent re-expansion (cycles).
--   - 'frontier' is a stack (list head = top). We push successors in front.
--   - Pre-order emission: we "visit" a node when we first pop it.
module Graph.DFS
    ( dfs        -- pure traversal (pre-order)
    , dfsTrace   -- concise step-by-step trace
    ) where

import qualified Data.Set as Set

-- Pure DFS traversal (pre-order).
-- succF: node -> neighbors
-- start: starting node
-- Returns the visit order.
dfs :: (Ord a) => (a -> [a]) -> a -> [a]
dfs succF start = go Set.empty [start]
    where
    go _    []     = []
    go seen (x:xs)
        | x `Set.member` seen = go seen xs
        | otherwise =
            let seen'     = Set.insert x seen
                neighbors = succF x         -- already sorted, no need to sort again
                frontier' = neighbors ++ xs  -- push onto stack (LIFO)
            in x : go seen' frontier'


-- Concise trace:
-- - "pop n -> visit; push [..]" when first seen
-- - "pop n -> skip (seen)" for repeats due to undirected back-edges or multi-edges
dfsTrace :: (Ord a, Show a) => (a -> [a]) -> a -> [String]
dfsTrace succF start = go Set.empty [start]
    where
    go _    []     = ["frontier empty -> stop"]
    go seen (x:xs)
        | x `Set.member` seen =
            ("pop " <> show x <> " -> skip (seen)") : go seen xs
        | otherwise =
            let neighbors = succF x
                seen'     = Set.insert x seen
                frontier' = neighbors ++ xs
            in  ("pop "  <> show x <> " -> visit")
            : ("push " <> show neighbors <> " (neighbors of " <> show x <> ")")
            : go seen' frontier'