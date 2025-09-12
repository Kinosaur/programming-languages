# graph-algo

Day 1 scope:
- Added pure DFS traversal (cycle-safe) using a successor function abstraction.

Run:
  stack build
  stack run

Inspect in ghci:
  stack ghci
  :load Graph.DFS
  dfs exampleSucc 1
  dfs' exampleSucc 1

Next steps (future days):
- Add path reconstruction for DFS (inefficient, illustrative).
- Implement BFS (naive list queue then Sequence-based).
- Introduce property-based testing (QuickCheck).