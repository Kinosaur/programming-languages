# Graph Search (DFS/BFS) Shared Spec

## Input format
- First line: "SRC DST"
- Each subsequent non-empty line: "U V" meaning an edge from U to V.
- Node names: strings without spaces. Avoid mixing case; tests use uppercase + digits.
- Duplicate edges may appear; treat adjacency as deduplicated.
- Blank lines are allowed; ignore them.
- Comments are NOT included in test files (keep parser simple).

## Directed vs Undirected datasets
- Directed datasets: interpret edges as U -> V as given.
- Undirected datasets: interpret each edge as bidirectional (add U -> V and V -> U).

## Exploration order (determinism requirement)
- Build adjacency by:
  1) collecting neighbors,
  2) deduplicating,
  3) sorting ascending lexicographically (string compare).
- When expanding a node, iterate neighbors in this sorted order.

## Algorithms
- DFS:
  - Depth-first, returns the first-found path under the neighbor order above.
  - Must track visited to avoid cycles.
  - Any valid path is acceptable if consistent with the specified neighbor order.
- BFS:
  - Classic queue-based BFS.
  - Mark visited when enqueuing neighbors (not when dequeuing).
  - Terminate when the target node is dequeued (this yields deterministic results under the order rule).
  - Reconstruct using a parent/predecessor map.

## Output format (exact)
- If a path exists: print a single line of the path as space-separated node names, from SRC to DST inclusive, e.g., "S A C T".
- If no path exists: print exactly "No path".

## Provided datasets
- Directed: d1_simple, d2_cycle_multi, d3_no_path, d4_tie_shortest, d5_long_vs_short_bias, d6_self_loop_dup
- Undirected: u1_simple, u2_two_paths, u3_long_vs_short_bias, u4_no_path, u5_triangle_direct_edge

Note: If your team changes neighbor ordering (e.g., insertion order instead of lexicographic), regenerate expected outputs accordingly. Consistency across languages matters more than the specific tie-break rule chosen.
