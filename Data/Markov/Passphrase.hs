module Data.Markov.Passphrase (passphrase, cleanForPassphrase) where

import Control.Arrow (first, (&&&), (***))
import Control.Monad.Random (Rand, RandomGen)
import Data.Char (toLower, isAlpha)
import Data.List (group, sort)
import Data.List.Split (split, whenElt, keepDelimsL)
import qualified Data.Map as Map

import Data.Markov
import Utils

entropy :: [Double] -> Double
entropy = negate . sum . map (\p -> p * logBase 2 p)

ngramDist :: (Eq a, Ord a) => [MarkovNode a] -> [Double]
ngramDist ns = map (/ sum counts) counts
  where
    counts = map (fromIntegral . length) . group . sort . map ngram $ ns

nodeEntropy :: (Eq a, Ord a) => MarkovNode a -> Double
nodeEntropy = entropy . ngramDist . nodes

wordsWithEntropy :: [MarkovNode Char] -> [(String, Double)]
wordsWithEntropy = getWords . map (token &&& nodeEntropy)
  where
    token = last . ngram
    getWords = map ((tail *** sum) . unzip) . splitWords
    splitWords = split . keepDelimsL . whenElt $ (== ' ') . fst

takeUntilAtLeast :: Double -> [(a, Double)] -> [(a, Double)]
takeUntilAtLeast n (x:xs)
  | n <= 0 = []
  | otherwise = x : takeUntilAtLeast (n - snd x) xs

firstNodeWithEntropy :: RandomGen g =>
  MarkovChain Char -> (Rand g (MarkovNode Char), Double)
firstNodeWithEntropy = (choice &&& entropy . ngramDist) . collectNodes
  where
    collectNodes = filter isWordStart . concatMap nodes . Map.elems
    isWordStart = (== ' ') . head . ngram

passphrase :: RandomGen g =>
  Double -> MarkovChain Char -> Rand g (String, Double)
passphrase eMin c = (first . (++) <$> s0) <*> (takeWords <$> iterateNodes n0)
  where
    (n0, e0) = firstNodeWithEntropy c
    s0 = tail . ngram <$> n0
    takeWords = mergeTuples . takeUntilAtLeast (eMin - e0) . wordsWithEntropy
    mergeTuples = (unwords *** (e0 +) . sum) . unzip

cleanForPassphrase :: String -> String
cleanForPassphrase = unwords . filter isGood . map clean . words
  where
    isGood word = length word > 4 && all isAlpha word
    clean = map toLower . reverse . lstrip . reverse . lstrip
    lstrip = takeWhile isAlpha
