{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Graph.DFS
  ( adjacencyOf,
    dfs,
    dfsTrace,
    exampleDirectedEdges,
    exampleUndirectedEdges,
    fromDirectedEdges,
    fromUndirectedEdges,
  )

-- Minimal numbered printer for the step trace.
printNumbered :: [String] -> IO ()
printNumbered steps = mapM_ putStrLn (zipWith (\i s -> show i <> ". " <> s) [(1 :: Int) ..] steps)

main :: IO ()
main = do
  let startNode = 1

  -- Directed
  let dirAdj = fromDirectedEdges exampleDirectedEdges
      succDir = adjacencyOf dirAdj
      orderDir = dfs succDir startNode
      traceDir = dfsTrace succDir startNode

  putStrLn "== DFS (DIRECTED) =="
  putStrLn $ "Edges: " <> show exampleDirectedEdges
  putStrLn $ "Start: " <> show startNode
  putStrLn $ "Order (visit sequence, smallest neighbor first each step): " <> show orderDir
  putStrLn "Steps:"
  printNumbered traceDir

  -- Undirected
  let undirAdj = fromUndirectedEdges exampleUndirectedEdges
      succUndir = adjacencyOf undirAdj
      orderUndir = dfs succUndir startNode
      traceUndir = dfsTrace succUndir startNode

  putStrLn "\n== DFS (UNDIRECTED) =="
  putStrLn $ "Edges: " <> show exampleUndirectedEdges
  putStrLn $ "Start: " <> show startNode
  putStrLn $ "Order (visit sequence, smallest neighbor first each step): " <> show orderUndir
  putStrLn "Steps:"
  printNumbered traceUndir

  putStrLn "\nNotes:"
  putStrLn "- \"Smallest first\" here means among the current node's neighbors; DFS still uses a stack (LIFO)."
  putStrLn "- If you actually want \"globally smallest next\" at every step, that is not DFS."