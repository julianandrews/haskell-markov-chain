module Data.Markov (MarkovChain, markovChain, generateTokens) where

import Control.Monad (liftM)
import Control.Monad.Random (Rand, getRandomR, RandomGen)
import Data.List (tails)
import qualified Data.Map as Map

type NGram a = [a]
data MarkovNode a = MarkovNode (NGram a) [MarkovNode a]
type MarkovChain a = Map.Map (NGram a) (MarkovNode a)

nGramMap :: Ord a => Int -> [a] -> Map.Map (NGram a) [NGram a]
nGramMap n = foldl addTransition Map.empty . toPairs . toNGrams n
  where
    addTransition m (from, to) = Map.insertWith (++) from [to] m
    toNGrams n xs = take (length xs - n + 1) . map (take n) . tails $ xs
    -- cycle guarantees that every node has at least one transition.
    toPairs l = zip l . tail . cycle $ l

markovChain :: (Eq a, Ord a) => Int -> [a] -> MarkovChain a
markovChain n tokens = chain
  where
    chain = Map.mapWithKey toMarkovNode . nGramMap n $ tokens
    toMarkovNode ngram = MarkovNode ngram . map (chain Map.!)

token :: MarkovNode a -> a
token (MarkovNode ngram nodes) = head ngram

choice :: RandomGen g => [a] -> Rand g a
choice elements = (elements !!) <$> getRandomR (0, length elements - 1)

nextNode :: RandomGen g => MarkovNode a -> Rand g (MarkovNode a)
nextNode (MarkovNode ngram nodes) = choice nodes

iterateM :: Monad m => (a -> m a) -> m a -> m [a]
iterateM step start = do
    first <- start
    rest <- iterateM step (step first)
    return (first:rest)

generateTokens :: RandomGen g => MarkovChain a -> Rand g [a]
generateTokens = liftM (map token) . iterateM nextNode . choice . Map.elems
