module Data.Markov.Passphrase (cleanForPassphrase, passphrase) where

import Control.Arrow (first, second, (&&&))
import Control.Monad.Random (Rand, RandomGen)
import Data.Char (toLower, isAlpha)
import Data.List (group, sort)
import Data.List.Split (split, whenElt, keepDelimsL)
import qualified Data.Map as Map
import Data.Markov

entropy :: [Double] -> Double
entropy = negate . sum . map (\p -> p * logBase 2 p)

nodeEntropy :: (Eq a, Ord a) => MarkovNode a -> Double
nodeEntropy (MarkovNode _ nodes) = entropy $ map (/ total) counts
  where
    counts = map (fromIntegral . length) . group . sort . map ngram $ nodes
    total = sum counts

wordsWithEntropy :: [MarkovNode Char] -> [(String, Double)]
wordsWithEntropy = map mergeTuples . splitWords . map (token &&& nodeEntropy)
  where
    mergeTuples = first (drop 1) . second sum . unzip
    splitWords = split . keepDelimsL . whenElt $ (== ' ') . fst

takeUntilAtLeast :: Double -> [(a, Double)] -> [(a, Double)]
takeUntilAtLeast n (x:xs)
  | n <= 0 = []
  | otherwise = x : takeUntilAtLeast (n - snd x) xs

cleanForPassphrase :: String -> String
cleanForPassphrase = unwords . filter isGood . map clean . words
  where
    isGood word = length word > 4 && all isAlpha word
    clean = map toLower . reverse . lstrip . reverse . lstrip
    lstrip = takeWhile isAlpha

passphrase :: RandomGen g =>
  Double -> MarkovChain Char -> Rand g (String, Double)
passphrase eMin chain = mergeTuples . takeEnough <$> iterateNodes (choice ns)
  where
    ns = filter ((==) ' ' . token) (Map.elems chain)
    e0 = entropy $ replicate l (1 / fromIntegral l)
      where l = length ns
    mergeTuples = first (drop 1 . unwords) . second ((e0 +) . sum) . unzip
    takeEnough = takeUntilAtLeast (eMin - e0) . wordsWithEntropy
