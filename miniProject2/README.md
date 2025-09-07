# Mini Project 2: Graph Search Test Suite

This folder contains deterministic test datasets and expected outputs for BFS and DFS on both directed and undirected graphs, following a strict spec for neighbor order and output format. Use these to validate your implementations across languages.

- All test files are in `data/`.
- See `data/GRAPH_SEARCH_SPEC.md` for the full specification and rules.
- Each test has a `.txt` input and `.bfs.out`/`.dfs.out` expected outputs.

## Structure

- `data/GRAPH_SEARCH_SPEC.md`: Full spec and rules
- `data/directed/` and `data/undirected/`: Test cases for each graph type
- `expected/`: Expected outputs for BFS and DFS

## Usage
- Run your BFS/DFS implementations on the `.txt` files
- Compare your output to the corresponding `.out` files
- All outputs must match exactly (including whitespace and order)

## Test Coverage
- Unique paths, multiple paths, cycles, ties, no-path, self-loops/duplicates, and cases where DFS finds a longer path than BFS

If you change neighbor order or output format, regenerate all expected outputs accordingly.
