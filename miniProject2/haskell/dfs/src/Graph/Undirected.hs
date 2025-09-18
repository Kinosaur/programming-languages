-- Parse undirected edge lists and build sorted, deduplicated adjacency.
module Graph.Undirected
    ( readUndirectedFile      -- IO: read file -> edges (u,v)
    , buildAdjacency          -- pure: edges -> Map node [sorted unique neighbors]
    , succOf                  -- pure: Map -> (node -> [neighbors])
    , nodesOf                 -- pure: Map -> [sorted unique nodes]
    , undirectedEdgeCount     -- pure: unique undirected edge count
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.Char (isSpace)
import           Data.List (sort)

type Adj a = Map.Map a [a]

-- Read lines like: "U V" (whitespace-separated). Ignores blank and comment (#) lines.
readUndirectedFile :: FilePath -> IO [(String, String)]
readUndirectedFile fp = do
    raw <- readFile fp
    let ls = lines raw
    pure (concatMap parseLine ls)
    where
    parseLine :: String -> [(String, String)]
    parseLine s =
        let t = dropWhile isSpace s
        in case t of
            []      -> []
            ('#':_) -> []     -- comment
            _       ->
                case words t of
                (u:v:_) -> [(u,v)]
                _       -> []  -- ignore malformed lines

-- Build adjacency with deduped, sorted neighbor lists.
-- For undirected graphs, add both u->v and v->u.
buildAdjacency :: (Ord a) => [(a,a)] -> Adj a
buildAdjacency es =
    let add u v m = Map.insertWith (\new old -> new ++ old) u [v] m
        m1 = foldr (\(u,v) m -> add u v (add v u m)) Map.empty es
        -- Dedup and sort neighbors
    in Map.map (sort . Set.toList . Set.fromList) m1

-- Successor function from adjacency map.
succOf :: (Ord a) => Adj a -> (a -> [a])
succOf m = \x -> Map.findWithDefault [] x m

-- All nodes present either as keys or neighbors, sorted.
nodesOf :: (Ord a) => Adj a -> [a]
nodesOf m =
    let ks = Set.fromList (Map.keys m)
        vs = Set.fromList (concat (Map.elems m))
    in sort (Set.toList (Set.union ks vs))

-- Count unique undirected edges (normalize (min,max) pairs).
undirectedEdgeCount :: (Ord a) => [(a,a)] -> Int
undirectedEdgeCount es =
    let norm (u,v) = if u <= v then (u,v) else (v,u)
    in Set.size (Set.fromList (map norm es))