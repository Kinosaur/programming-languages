# DFS (Directed and Undirected, Haskell)

## Overview
This project implements Depth-First Search (DFS) for both directed and undirected graphs in Haskell. The program reads graph data from an input file, performs DFS, and outputs the path from a source node to a destination node.

## Run Instructions
1. Build the project:
   ```
   stack build
   ```
2. Run the program with the desired input file and mode:
   ```
   stack run -- --mode directed data/directed/d1_simple.dfs.in
   stack run -- --mode undirected data/undirected/u1_simple.dfs.in
   ```

## Input Format
- The input file should follow this format:
  1. First line: `N M` (number of nodes and edges).
  2. Second line: `SRC DST` (source and destination nodes).
  3. Next `M` lines: `U V` (edges between nodes).
- Blank lines are ignored.

## Features
- **Directed and Undirected Graphs**: Specify the mode using `--mode directed` or `--mode undirected`.
- **Neighbor Order**: DFS visits neighbors in ascending numerical order.
- **Output**: Prints the path from the source to the destination as a space-separated list of nodes, or `No path` if no path exists.

## Example Usage
### Directed Graph
Input file: `data/directed/d1_simple.dfs.in`
```
5 4
1 5
1 2
2 3
3 5
4 5
```
Run:
```
stack run -- --mode directed data/directed/d1_simple.dfs.in
```
Output:
```
1 2 3 5
```

### Undirected Graph
Input file: `data/undirected/u1_simple.dfs.in`
```
4 3
1 4
1 2
2 3
3 4
```
Run:
```
stack run -- --mode undirected data/undirected/u1_simple.dfs.in
```
Output:
```
1 2 3 4
```

## Notes
- The program automatically detects the graph type based on the `--mode` flag.
- Self-loops and duplicate edges are handled gracefully.
- Test cases are provided in the `data/` directory for validation.