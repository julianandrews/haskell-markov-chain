module Data.Markov.Entropy (chainEntropy, tokensWithEntropy) where

import Control.Arrow ((&&&))
import Control.Monad.Random (Rand, RandomGen)
import Data.List (group, sort)
import qualified Data.Map as Map
import Data.Markov

entropy :: [Double] -> Double
entropy = negate . sum . map (\p -> p * logBase 2 p)

chainEntropy :: MarkovChain a -> Double
chainEntropy = entropy . equalDistribution . length . Map.elems
  where
    equalDistribution l = replicate l (1 / fromIntegral l)

nodeEntropy :: (Eq a, Ord a) => MarkovNode a -> Double
nodeEntropy (MarkovNode _ nodes) = entropy $ map probability counts
  where
    counts = map length . group . sort . map ngram $ nodes
    total = sum counts
    probability count = fromIntegral count / fromIntegral total

tokensWithEntropy :: (Eq a, Ord a, RandomGen g) => MarkovChain a -> Rand g [(a, Double)]
tokensWithEntropy = (map (token &&& nodeEntropy) <$>) . iterateNodes
