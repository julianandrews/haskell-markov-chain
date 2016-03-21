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
nodeEntropy (MarkovNode _ nodes) = entropy $ map probability counts
  where
    counts = map length . group . sort . map ngram $ nodes
    total = sum counts
    probability count = fromIntegral count / fromIntegral total

wordsWithEntropy :: [MarkovNode Char] -> [(String, Double)]
wordsWithEntropy = map summarizeWord . splitWords . map (token &&& nodeEntropy)
  where
    summarizeWord = first (drop 1) . second sum . unzip
    splitWords = split . keepDelimsL . whenElt $ (== ' ') . fst

takeUntilTotal :: Double -> [(a, Double)] -> [(a, Double)]
takeUntilTotal n (x:xs)
  | n <= 0 = []
  | otherwise = x : takeUntilTotal (n - snd x) xs

cleanForPassphrase :: String -> String
cleanForPassphrase = unwords . filter ((>5) . length) . map cleanWord . words
  where
    cleanWord = map toLower . takeWhile isAlpha . dropWhile (not . isAlpha)

passphrase :: RandomGen g =>
  Double -> MarkovChain Char -> Rand g (String, Double)
passphrase minEntropy chain = combine . takeNeededWords <$> iterateNodes (choice startingNodes)
  where
    startingNodes = filter ((==) ' ' . token) (Map.elems chain)
    startingNodeCount = length startingNodes
    startingEntropy = entropy $ replicate startingNodeCount (1 / fromIntegral startingNodeCount)
    combine = first (unwords . drop 1) . second ((+ startingEntropy) . sum) . unzip
    takeNeededWords = takeUntilTotal (minEntropy - startingEntropy) . wordsWithEntropy
