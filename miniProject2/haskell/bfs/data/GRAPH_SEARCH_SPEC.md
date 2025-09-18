# Graph Search (DFS/BFS) — N M + Edges Format

## Input format
- Line 1: `N M`
  - `N`: number of nodes (IDs are integers `1..N`)
  - `M`: number of edges (edge lines follow; duplicates allowed)
- Line 2: `SRC DST`
  - Source and destination node IDs
- Next `M` lines: edges `U V`
  - Directed datasets: interpret as `U -> V`
  - Undirected datasets: treat each edge as bidirectional (`U -> V` and `V -> U`)
- No comments. Blank lines are allowed and ignored.
- Duplicate edges may appear; adjacency must be deduplicated.

## Exploration order (determinism)
- Build adjacency by:
  1) collecting neighbors,
  2) deduplicating,
  3) sorting ascending numerically.
- When expanding a node, iterate neighbors in this sorted order.

## Algorithms
- DFS:
  - Depth-first; mark visited on entry (preorder).
  - Return the first-found path from `SRC` to `DST` under the neighbor order above.
- BFS:
  - Queue-based BFS.
  - Mark visited when enqueuing neighbors (not when dequeuing).
  - Terminate when the target node is dequeued.
  - Reconstruct using a parent/predecessor map.

## Output format (exact)
- If a path exists: print a single line of the path as space-separated node IDs from `SRC` to `DST`, e.g., `1 3 5`.
- If no path exists: print exactly `No path`.

## Folder structure
- `directed/`
  - `<dataset>.bfs.in`
  - `<dataset>.dfs.in`
  - `expected/`
    - `<dataset>.bfs.out`
    - `<dataset>.dfs.out`
- `undirected/`
  - same pattern as above

## Notes
- Numeric sort avoids cross-language locale/string differences.
- For undirected graphs, add reverse edges then deduplicate adjacency.
- The BFS “stop on dequeue” rule plus numeric neighbor order yields deterministic results across implementations.