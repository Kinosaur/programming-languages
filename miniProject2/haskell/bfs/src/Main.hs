{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M

import Graph.Format (LabeledInput(..), readLabeledInput)
import qualified Graph.DirectedBFS   as Dir
import qualified Graph.UndirectedBFS as Undir
import Graph.BFS (bfsPath)

data Mode = Directed | Undirected deriving (Eq, Show)

errInvalidArgs :: String
errInvalidArgs = "Error: invalid arguments."

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  stack run -- --mode directed   <path/to/DG.in>"
  putStrLn "  stack run -- --mode undirected <path/to/UDG.in>"
  putStrLn ""
  putStrLn "Input format:"
  putStrLn "  N M"
  putStrLn "  SRC DST"
  putStrLn "  M lines: U V"
  putStrLn "Nodes are arbitrary labels (no spaces)."
  putStrLn "Output: space-separated path labels, or 'No path'."

parseMode :: String -> Maybe Mode
parseMode s =
  case map toLower s of
    "directed"   -> Just Directed
    "undirected" -> Just Undirected
    _            -> Nothing

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
  LabeledInput nDecl mDecl sLbl dLbl ledges <- readLabeledInput fp
  let allLabels = uniqPreserve (sLbl:dLbl:concatMap (\(a,b)->[a,b]) ledges)
      labelCount = length allLabels
  if labelCount /= nDecl
    then do
      putStrLn ("Error: declared N=" ++ show nDecl ++ " but counted " ++ show labelCount ++ " labels.")
      exitFailure
    else do
      let labelToInt = M.fromList (zip allLabels [0..])
          intToLabel = M.fromList (zip [0..] allLabels)
          enc (a,b) =
            case (M.lookup a labelToInt, M.lookup b labelToInt) of
              (Just ai, Just bi) -> (ai, bi)
              _ -> error "Internal mapping failure."
          edgesInt = map enc ledges
          srcInt   = labelToInt M.! sLbl
          dstInt   = labelToInt M.! dLbl
          runDir = do
            let adj = Dir.buildAdjacency edgesInt
                succF = Dir.succOf adj
            outputPath intToLabel (bfsPath succF srcInt dstInt)
          runUndir = do
            let adj = Undir.buildAdjacency edgesInt
                succF = Undir.succOf adj
            outputPath intToLabel (bfsPath succF srcInt dstInt)
      case mode of
        Directed   -> runDir
        Undirected -> runUndir
  where
    outputPath :: M.Map Int String -> Maybe [Int] -> IO ()
    outputPath intToLabel mp =
      case mp of
        Nothing  -> putStrLn "No path"
        Just is  -> putStrLn (unwords (map (\i -> intToLabel M.! i) is))

    uniqPreserve :: (Ord a) => [a] -> [a]
    uniqPreserve = go M.empty []
      where
        go _ acc [] = reverse acc
        go seen acc (x:xs) =
          if M.member x seen
             then go seen acc xs
             else go (M.insert x () seen) (x:acc) xs