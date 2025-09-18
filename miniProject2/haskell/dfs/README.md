# DFS (Undirected, Haskell)

Run:
  stack build
  stack run -- data/undirected/u1_simple.txt S

Defaults:
- If no start node is given, uses "S" if present, else the smallest node.
- DFS visits smallest neighbor first (lexicographic on node labels).

Input format:
- One undirected edge per line: "U V"
- Blank lines and lines starting with "#" are ignored.