# Changelog

## [Unreleased]

### Added
- Implemented DFS (Depth-First Search) for directed and undirected graphs.
- Added `Graph.Format` module to parse input files in `N M + SRC DST + M edges` format.
- Added `Graph.Directed` and `Graph.Undirected` modules to handle adjacency list construction.
- Added `Graph.DFS` module for DFS path search with deduped and sorted neighbors.
- Command-line interface in `Main.hs` to specify graph mode (`--mode directed` or `--mode undirected`) and input file.

### Changed
- Updated `stack.yaml` to include `ghc-options` for linker warnings.
- Updated `dfs.cabal` to include all modules and ensure compatibility.

### Testing
- Test cases for both directed and undirected graphs are included in the `data/` directory.
- Verified output matches expected results for all test cases.