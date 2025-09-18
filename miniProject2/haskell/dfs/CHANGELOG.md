# Changelog for `dfs`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased
### Added
- CLI now supports `--mode` flag for specifying directed or undirected mode explicitly.
- Updated usage instructions in `Main.hs`.
- Improved argument parsing to handle `--mode=<val>` and `-m <val>` formats.

### Changed
- Refactored `Main.hs` to use explicit mode flags instead of path inference.

## 0.1.0.0
- Initial undirected DFS with sorted neighbors, simple trace, and file parsing.