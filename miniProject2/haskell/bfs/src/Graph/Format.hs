{-# OPTIONS_GHC -Wall #-}
module Graph.Format
  ( NMInput(..)
  , readNMInput
  ) where

import Data.Char (isSpace)

data NMInput = NMInput
  { n     :: Int
  , m     :: Int
  , src   :: Int
  , dst   :: Int
  , edges :: [(Int, Int)]
  } deriving (Show, Eq)

-- Reads the N M + SRC DST + M edges format.
-- - Ignores blank lines anywhere.
-- - Takes the first two non-blank lines as headers, then the next M non-blank lines as edges.
readNMInput :: FilePath -> IO NMInput
readNMInput fp = do
  contents <- readFile fp
  let ls        = lines contents
      cleaned   = filter (not . null) (map trim ls)
  case cleaned of
    (l1:l2:rest) ->
      let (ni, mi)  = parsePair2 l1
          (s, d)    = parsePair2 l2
          edgeLines = take mi rest
          es        = map parsePair2 edgeLines
      in if length edgeLines /= mi
           then error "Invalid input: fewer edge lines than M."
           else pure NMInput { n = ni, m = mi, src = s, dst = d, edges = es }
    _ -> error "Invalid input: need at least two non-blank lines (N M) and (SRC DST)."
  where
    parsePair2 :: String -> (Int, Int)
    parsePair2 s =
      case map read (words s) of
        [a,b] -> (a,b)
        _     -> error ("Invalid line (need two integers): " <> s)

    trim :: String -> String
    trim = dropWhile isSpace . dropWhileEnd isSpace

    dropWhileEnd :: (a -> Bool) -> [a] -> [a]
    dropWhileEnd p = reverse . dropWhile p . reverse