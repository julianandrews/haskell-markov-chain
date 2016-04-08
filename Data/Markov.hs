module Data.Markov (
  MarkovChain, MarkovNode(..), markovChain, iterateNodes
) where

import Control.Monad.Random (Rand, RandomGen)
import Data.List (tails)
import qualified Data.Map as Map

import Utils (choice)

type NGram a = [a]
data MarkovNode a = MarkovNode {ngram :: NGram a, nodes :: [MarkovNode a]}
type MarkovChain a = Map.Map (NGram a) (MarkovNode a)

ngramMap :: Ord a => Int -> [a] -> Map.Map (NGram a) [NGram a]
ngramMap n = foldl addTransition Map.empty . toPairs . toNGrams n
  where
    addTransition m (from, to) = Map.insertWith (++) from [to] m
    toNGrams n xs = take (length xs - n + 1) . map (take n) . tails $ xs
    toPairs l = zip l . tail . cycle $ l

markovChain :: (Eq a, Ord a) => Int -> [a] -> MarkovChain a
markovChain n tokens = chain
  where
    chain = Map.mapWithKey toMarkovNode . ngramMap n $ tokens
    toMarkovNode n = MarkovNode n . map (chain Map.!)

iterateM :: Monad m => (a -> m a) -> m a -> m [a]
iterateM step start = do
    first <- start
    rest <- iterateM step (step first)
    return (first:rest)

iterateNodes :: RandomGen g => Rand g (MarkovNode a) -> Rand g [MarkovNode a]
iterateNodes = iterateM $ choice . nodes
