{-# OPTIONS_GHC -Wall #-}
module Graph.Format
  ( LabeledInput(..)
  , readLabeledInput
  ) where

import Data.Char (isSpace)

data LabeledInput = LabeledInput
  { nDecl  :: Int
  , mDecl  :: Int
  , srcLbl :: String
  , dstLbl :: String
  , edgesL :: [(String, String)]
  } deriving (Show, Eq)

-- Reads format:
--   N M
--   SRC DST
--   M lines: U V
-- Blank lines anywhere are ignored.
readLabeledInput :: FilePath -> IO LabeledInput
readLabeledInput fp = do
  contents <- readFile fp
  let ls      = lines contents
      cleaned = filter (not . null) (map trim ls)
  case cleaned of
    (l1:l2:rest) ->
      let (ni, mi)  = parsePair2Ints l1
          (s,d)     = parsePair2Labels l2
          edgeLines = take mi rest
      in if length edgeLines /= mi
            then fail "Invalid input: fewer edge lines than M."
            else do
              let es = map parsePair2Labels edgeLines
                  allLabels = unique (s:d:[ u | (u,_) <- es ] ++ [ v | (_,v) <- es ])
                  distinctCount = length allLabels
              if distinctCount /= ni
                then fail ("Declared N does not match distinct label count: declared=" ++ show ni ++ " actual=" ++ show distinctCount)
                else pure LabeledInput
                       { nDecl = ni
                       , mDecl = mi
                       , srcLbl = s
                       , dstLbl = d
                       , edgesL = es
                       }
    _ -> fail "Invalid input: need at least two non-blank lines."
  where
    parsePair2Ints :: String -> (Int, Int)
    parsePair2Ints s =
      case words s of
        [a,b] -> (read a, read b)
        _     -> error ("Invalid N M line: " <> s)

    parsePair2Labels :: String -> (String, String)
    parsePair2Labels s =
      case words s of
        [a,b] -> (a,b)
        _     -> error ("Invalid line (need two tokens): " <> s)

    trim :: String -> String
    trim = dropWhile isSpace . dropWhileEnd isSpace

    dropWhileEnd :: (a -> Bool) -> [a] -> [a]
    dropWhileEnd p = reverse . dropWhile p . reverse

    -- Deterministic unique (preserves first occurrence)
    unique :: (Ord a) => [a] -> [a]
    unique = go mempty
      where
        go _ [] = []
        go seen (x:xs)
          | x `elemSet` seen = go seen xs
          | otherwise        = x : go (insertSet x seen) xs

    -- Simple Set-like helpers without importing Data.Set (small lists OK).
    elemSet :: (Eq a) => a -> [a] -> Bool
    elemSet = elem

    insertSet :: a -> [a] -> [a]
    insertSet x xs = x:xs