{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           Data.Char          (toLower)
import           Data.List          (isPrefixOf)

import           Graph.Format     (NMInput(..), readNMInput)
import qualified Graph.Directed   as Dir
import qualified Graph.Undirected as Undir
import           Graph.BFS        (bfsPath)

data Mode = Directed | Undirected deriving (Eq, Show)

errInvalidArgs :: String
errInvalidArgs = "Error: invalid arguments."

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  stack run -- --mode directed   <path/to/input.in>"
  putStrLn "  stack run -- --mode undirected <path/to/input.in>"
  putStrLn ""
  putStrLn "Input format: N M, then SRC DST, then M lines of edges U V (integers)."
  putStrLn "Output: space-separated path from SRC to DST, or 'No path'."

parseMode :: String -> Maybe Mode
parseMode s =
  case map toLower s of
    "directed"   -> Just Directed
    "undirected" -> Just Undirected
    _            -> Nothing

-- Accepts:
--   --mode <val> <file>
--   -m <val> <file>
--   --mode=<val> <file>
parseArgs :: [String] -> Either String (Mode, FilePath)
parseArgs ["--mode", modeStr, fp]   = maybe (Left errInvalidArgs) (\mm -> Right (mm, fp)) (parseMode modeStr)
parseArgs ["-m", modeStr, fp]       = maybe (Left errInvalidArgs) (\mm -> Right (mm, fp)) (parseMode modeStr)
parseArgs [opt, fp]
  | "--mode=" `isPrefixOf` opt =
      let mstr = drop (length "--mode=") opt
      in maybe (Left errInvalidArgs) (\mm -> Right (mm, fp)) (parseMode mstr)
parseArgs _ = Left errInvalidArgs

main :: IO ()
main = do
  e <- parseArgs <$> getArgs
  case e of
    Left err -> do
      putStrLn err
      usage
      exitFailure
    Right (mode, fp) -> run mode fp

run :: Mode -> FilePath -> IO ()
run mode fp = do
  NMInput _ _ s d es <- readNMInput fp
  case mode of
    Directed   -> runDirected s d es
    Undirected -> runUndirected s d es

runDirected :: Int -> Int -> [(Int,Int)] -> IO ()
runDirected s d es =
  let adj   = Dir.buildAdjacency es
      succF = Dir.succOf adj
  in case bfsPath succF s d of
       Nothing  -> putStrLn "No path"
       Just pth -> putStrLn (unwords (map show pth))

runUndirected :: Int -> Int -> [(Int,Int)] -> IO ()
runUndirected s d es =
  let adj   = Undir.buildAdjacency es
      succF = Undir.succOf adj
  in case bfsPath succF s d of
       Nothing  -> putStrLn "No path"
       Just pth -> putStrLn (unwords (map show pth))