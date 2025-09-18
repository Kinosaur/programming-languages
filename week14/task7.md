## Explanation (Task 7)

### Why Prolog Sometimes Loops Infinitely

Prolog uses a depth-first search (DFS) strategy by default. Without tracking visited states, the boat can repeatedly shuttle between the same configurations (e.g., moving out and then back), causing infinite recursion—even when a solution exists. With a boat capacity of 3, the branching factor increases, and many moves are reversible, making cycles even more likely to trap a naive DFS.

### How to Avoid Revisiting States

To prevent cycles, maintain a list of visited states and avoid expanding successors already in this list. Since the state space is finite for fixed totals, this approach guarantees termination. In this file, the predicate `dfs/4` carries the `Visited` list and uses `member(Next, Visited)` to prune cycles.