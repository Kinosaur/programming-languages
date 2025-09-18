# BFS (Haskell) — N M + Edges format

Run:
- Directed: `stack run -- --mode directed   data/directed/d1_simple.in`
- Undirected: `stack run -- --mode undirected data/undirected/u1_simple.in`

Spec:
- Input: line1 `N M`, line2 `SRC DST`, then `M` edges `U V`.
- Undirected: add reverse edges then dedupe.
- Determinism: neighbors deduped and sorted ascending numerically.
- BFS:
  - mark visited on enqueue
  - stop when target is dequeued
  - reconstruct path using parent map
- Output: either `u1 u2 ... uk` or `No path`.