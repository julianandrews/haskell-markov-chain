module Data.Markov (
  MarkovChain,
  MarkovNode(..),
  markovChain,
  ngram,
  token,
  iterateNodes,
  iterateTokens
) where

import Control.Monad.Random (Rand, getRandomR, RandomGen)
import Data.List (tails)
import qualified Data.Map as Map

type NGram a = [a]
data MarkovNode a = MarkovNode (NGram a) [MarkovNode a] deriving (Eq)
type MarkovChain a = Map.Map (NGram a) (MarkovNode a)

nGramMap :: Ord a => Int -> [a] -> Map.Map (NGram a) [NGram a]
nGramMap n = foldl addTransition Map.empty . toPairs . toNGrams n
  where
    addTransition m (from, to) = Map.insertWith (++) from [to] m
    toNGrams n xs = take (length xs - n + 1) . map (take n) . tails $ xs
    -- `cycle` guarantees that every node has at least one transition.
    toPairs l = zip l . tail . cycle $ l

markovChain :: (Eq a, Ord a) => Int -> [a] -> MarkovChain a
markovChain n tokens = chain
  where
    chain = Map.mapWithKey toMarkovNode . nGramMap n $ tokens
    toMarkovNode n = MarkovNode n . map (chain Map.!)

ngram :: MarkovNode a -> NGram a
ngram (MarkovNode n _) = n

token :: MarkovNode a -> a
token = head . ngram

choice :: RandomGen g => [a] -> Rand g a
choice elements = (elements !!) <$> getRandomR (0, length elements - 1)

next :: RandomGen g => MarkovNode a -> Rand g (MarkovNode a)
next (MarkovNode _ nodes) = choice nodes

iterateM :: Monad m => (a -> m a) -> m a -> m [a]
iterateM step start = do
    first <- start
    rest <- iterateM step (step first)
    return (first:rest)

iterateNodes :: RandomGen g => MarkovChain a -> Rand g [MarkovNode a]
iterateNodes = iterateM next . choice . Map.elems

iterateTokens :: RandomGen g => MarkovChain a -> Rand g [a]
iterateTokens = (map token <$>) . iterateNodes
