{-# OPTIONS_GHC -Wall #-}

-- Parse directed edge lists and build sorted, deduplicated adjacency.
-- Designed to match the API of Graph.Undirected so you can swap modules in Main.
module Graph.Directed
    ( readDirectedFile, -- IO: read file -> edges (u,v)
    buildAdjacency, -- pure: edges -> Map node [sorted unique OUT-neighbors]
    succOf, -- pure: Map -> (node -> [neighbors])
    nodesOf, -- pure: Map -> [sorted unique nodes] (sources ∪ targets)
    directedEdgeCount, -- pure: unique directed arc count
    )
where

import Data.Char (isSpace)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Adj a = Map.Map a [a]

-- Read lines like: "U V" (whitespace-separated). Ignores blank and comment (#) lines.
-- Interprets each line as a directed edge U -> V.
readDirectedFile :: FilePath -> IO [(String, String)]
readDirectedFile fp = do
    raw <- readFile fp
    let ls = lines raw
    pure (concatMap parseLine ls)
    where
    parseLine :: String -> [(String, String)]
    parseLine s =
        let t = dropWhile isSpace s
        in case t of
            [] -> []
            ('#' : _) -> [] -- comment
            _ ->
                case words t of
                (u : v : _) -> [(u, v)]
                _ -> [] -- ignore malformed lines

-- Build adjacency with deduped, sorted neighbor lists.
-- Only inserts U -> V (single direction). Self-loops are preserved.
buildAdjacency :: (Ord a) => [(a, a)] -> Adj a
buildAdjacency es =
    let add u v m = Map.insertWith (\new old -> new ++ old) u [v] m
        m1 = foldr (\(u, v) m -> add u v m) Map.empty es
    in Map.map (sort . Set.toList . Set.fromList) m1

-- Successor function from adjacency map.
succOf :: (Ord a) => Adj a -> (a -> [a])
succOf m = \x -> Map.findWithDefault [] x m

-- All nodes present either as sources (keys) or targets (neighbors), sorted.
-- This ensures nodes that only have incoming edges still appear.
nodesOf :: (Ord a) => Adj a -> [a]
nodesOf m =
    let ks = Set.fromList (Map.keys m)
        vs = Set.fromList (concat (Map.elems m))
    in sort (Set.toList (Set.union ks vs))

-- Count unique directed arcs (u,v), deduped.
directedEdgeCount :: (Ord a) => [(a, a)] -> Int
directedEdgeCount es = Set.size (Set.fromList es)