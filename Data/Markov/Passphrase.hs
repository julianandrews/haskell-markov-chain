module Data.Markov.Passphrase (passphrase, cleanForPassphrase) where

import Control.Arrow (first, (&&&), (***))
import Control.Monad.Random (Rand, RandomGen)
import Data.Char (toLower, isAlpha)
import Data.List (group, sort, sortBy)
import Data.List.Split (split, whenElt, keepDelimsL)
import qualified Data.Map as Map

import Data.Markov
import Utils

work :: [Double] -> Double
work = negate . sum . map (\p -> p * logBase 2 p)

ngramDist :: (Eq a, Ord a) => [MarkovNode a] -> [Double]
ngramDist ns = map (/ sum counts) counts
  where
    counts = map (fromIntegral . length) . group . sort . map ngram $ ns

nodeWork :: (Eq a, Ord a) => MarkovNode a -> Double
nodeWork = work . ngramDist . nodes

wordsWithWork :: [MarkovNode Char] -> [(String, Double)]
wordsWithWork = getWords . map (token &&& nodeWork)
  where
    token = last . ngram
    getWords = map ((tail *** sum) . unzip) . filter (/= []) . splitWords
    splitWords = split . keepDelimsL . whenElt $ (== ' ') . fst

takeUntilAtLeast :: Double -> [(a, Double)] -> [(a, Double)]
takeUntilAtLeast n (x:xs)
  | n <= 0 = []
  | otherwise = x : takeUntilAtLeast (n - snd x) xs

firstNodeWithWork :: RandomGen g =>
  MarkovChain Char -> (Rand g (MarkovNode Char), Double)
firstNodeWithWork = (choice &&& work . ngramDist) . collectNodes
  where
    collectNodes = filter isWordStart . concatMap nodes . Map.elems
    isWordStart = (== ' ') . head . ngram

passphrase :: RandomGen g =>
  Double -> MarkovChain Char -> Rand g (String, Double)
passphrase eMin c = (first . (++) <$> s0) <*> (takeWords <$> iterateNodes n0)
  where
    (n0, e0) = firstNodeWithWork c
    s0 = tail . ngram <$> n0
    takeWords = mergeTuples . takeUntilAtLeast (eMin - e0) . wordsWithWork
    mergeTuples = (unwords *** (e0 +) . sum) . unzip

cleanForPassphrase :: Int -> String -> String
cleanForPassphrase minWordLength = (' ' :) . unwords . cleanWords . words
  where
    cleanWords = filter isGood . map clean
    isGood word = length word >= minWordLength && all isAlpha word
    clean = map toLower . reverse . lstrip . reverse . lstrip
    lstrip = takeWhile isAlpha
