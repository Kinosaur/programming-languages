{-# OPTIONS_GHC -Wall #-}
module Graph.Format
  ( LabeledInput(..)
  , readLabeledInput
  ) where

import Data.Char (isSpace)

-- Represents labeled graph input:
--   N M
--   SRC DST
--   M lines: U V
data LabeledInput = LabeledInput
  { nDecl     :: Int
  , mDecl     :: Int
  , srcLabel  :: String
  , dstLabel  :: String
  , edgeLabels :: [(String, String)]
  } deriving (Show, Eq)

readLabeledInput :: FilePath -> IO LabeledInput
readLabeledInput fp = do
  contents <- readFile fp
  let ls        = lines contents
      cleaned   = filter (not . null) (map trim ls)
  case cleaned of
    (l1:l2:rest) ->
      let (ni, mi)  = parseTwoInts l1
          (s, d)    = parseTwoLabels l2
          edgeLines = take mi rest
      in if length edgeLines /= mi
           then fail "Invalid input: number of edge lines does not match M."
           else do
             let es = map parseTwoLabels edgeLines
                 allLabels = uniqPreserve (s : d : concatMap (\(a,b)->[a,b]) es)
                 distinctCount = length allLabels
             if distinctCount /= ni
               then fail ("Declared N does not match distinct label count. Declared=" ++ show ni
                          ++ " Actual=" ++ show distinctCount)
               else pure LabeledInput
                     { nDecl = ni
                     , mDecl = mi
                     , srcLabel = s
                     , dstLabel = d
                     , edgeLabels = es
                     }
    _ -> fail "Invalid input: need at least two non-blank lines."
  where
    parseTwoInts :: String -> (Int, Int)
    parseTwoInts s =
      case words s of
        [a,b] -> (read a, read b)
        _     -> error ("Invalid N M line: " <> s)

    parseTwoLabels :: String -> (String, String)
    parseTwoLabels s =
      case words s of
        [a,b] -> (a,b)
        _     -> error ("Invalid line (need two tokens): " <> s)

    trim :: String -> String
    trim = dropWhile isSpace . dropWhileEnd isSpace

    dropWhileEnd :: (a -> Bool) -> [a] -> [a]
    dropWhileEnd p = reverse . dropWhile p . reverse

    uniqPreserve :: (Ord a) => [a] -> [a]
    uniqPreserve = go [] []
      where
        go _ acc [] = reverse acc
        go seen acc (x:xs)
          | x `elem` seen = go seen acc xs
          | otherwise     = go (x:seen) (x:acc) xs