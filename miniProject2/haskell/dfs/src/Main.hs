{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import           System.Environment (getArgs)
import           Data.List (intercalate)

import           Graph.DFS (dfs, dfsTrace)
import           Graph.Undirected
  ( readUndirectedFile
  , buildAdjacency
  , succOf
  , nodesOf
  , undirectedEdgeCount
  )

usage :: IO ()
usage = do
  putStrLn "Usage: stack run -- <edge-file> [start-node]"
  putStrLn "  <edge-file>: path like data/undirected/u1_simple.txt"
  putStrLn "  [start-node]: optional; default 'S' if present, else smallest node"

pickStart :: [String] -> String
pickStart ns =
  case (elem "S" ns, ns) of
    (True, _)   -> "S"
    (_, x:_)    -> x
    _           -> error "No nodes found in graph."

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fp]      -> run fp Nothing
    [fp,s]    -> run fp (Just s)
    _         -> usage
  where
    run fp mStart = do
      es <- readUndirectedFile fp
      let adj      = buildAdjacency es
          succF    = succOf adj
          ns       = nodesOf adj
          ecount   = undirectedEdgeCount es
          start    = maybe (pickStart ns) id mStart
      putStrLn $ "Loaded undirected graph from: " <> fp
      putStrLn $ "Nodes (" <> show (length ns) <> "): " <> intercalate " " ns
      putStrLn $ "Unique undirected edges: " <> show ecount
      putStrLn $ "Start node: " <> show start
      putStrLn ""

      let order = dfs succF start
      putStrLn $ "DFS visit order (smallest neighbor first): " <> show order
      putStrLn "Steps:"
      mapM_ putStrLn (zipWith (\i s -> show i <> ". " <> s) [(1::Int)..] (dfsTrace succF start))