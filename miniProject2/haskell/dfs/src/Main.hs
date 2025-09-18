{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Char (toLower)
import Data.List (isPrefixOf)

import Graph.Format (NMInput(..), readNMInput)
import qualified Graph.Directed as Dir
import qualified Graph.Undirected as Undir
import Graph.DFS (dfsPath)

data Mode = Directed | Undirected deriving (Eq, Show)

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  stack run -- --mode directed <path/to/input.in>"
  putStrLn "  stack run -- --mode undirected <path/to/input.in>"
  putStrLn ""
  putStrLn "Input format: N M, then SRC DST, then M lines of edges U V (integers)."
  putStrLn "Output: space-separated path from SRC to DST, or 'No path'."

parseMode :: String -> Maybe Mode
parseMode s =
  case map toLower s of
    "directed" -> Just Directed
    "undirected" -> Just Undirected
    _ -> Nothing

-- Accepts:
--   --mode <val> <file>
--   -m <val> <file>
--   --mode=<val> <file>
parseArgs :: [String] -> Either String (Mode, FilePath)
parseArgs ["--mode", modeStr, filePath] = maybe (Left "Error: invalid arguments.") (\parsedMode -> Right (parsedMode, filePath)) (parseMode modeStr)
parseArgs ["-m", modeStr, filePath] = maybe (Left "Error: invalid arguments.") (\parsedMode -> Right (parsedMode, filePath)) (parseMode modeStr)
parseArgs [opt, filePath]
  | "--mode=" `isPrefixOf` opt =
      let modeStr = drop (length "--mode=") opt
      in maybe (Left "Error: invalid arguments.") (\parsedMode -> Right (parsedMode, filePath)) (parseMode modeStr)
parseArgs _ = Left "Error: invalid arguments."

main :: IO ()
main = do
  e <- parseArgs <$> getArgs
  case e of
    Left err -> do
      putStrLn err
      usage
      exitFailure
    Right (mode, filePath) -> run mode filePath

run :: Mode -> FilePath -> IO ()
run mode filePath = do
  nmInput <- readNMInput filePath
  case mode of
    Directed -> runDirected nmInput
    Undirected -> runUndirected nmInput

runDirected :: NMInput -> IO ()
runDirected (NMInput _ _ srcNode dstNode edgeList) =
  let adj = Dir.buildAdjacency edgeList
      succF = Dir.succOf adj
  in case dfsPath succF srcNode dstNode of
        Nothing -> putStrLn "No path"
        Just path -> putStrLn (unwords (map show path))

runUndirected :: NMInput -> IO ()
runUndirected (NMInput _ _ srcNode dstNode edgeList) =
  let adj = Undir.buildAdjacency edgeList
      succF = Undir.succOf adj
  in case dfsPath succF srcNode dstNode of
        Nothing -> putStrLn "No path"
        Just path -> putStrLn (unwords (map show path))