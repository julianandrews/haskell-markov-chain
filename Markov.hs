module Markov (markovChain, generateTokens) where

import qualified Data.Map as Map
import Data.Random.Extras (choice)
import System.Random (RandomGen, randomR)

-- Example: 
-- :m + System.Random
-- let n = markovChain ["Tic", "Toc", "Tic", "Toc", "Tac"] Map.! ("Tic", "Toc")
-- take 20 $ generateTokens n (mkStdGen 42)

type NGram a = (a, a)
data MarkovNode a = MarkovNode (NGram a) [MarkovNode a]
type MarkovChain a = Map.Map (NGram a) (MarkovNode a)

nGramMap :: Ord a => [a] -> Map.Map (NGram a) [NGram a]
nGramMap = foldl addTransition Map.empty . toPairs . toNGrams
  where
    addTransition m (from, to) = Map.insertWith (++) from [to] m
    toNGrams = toPairs
    toPairs l = zip l $ tail $ cycle l

markovChain :: (Eq a, Ord a) => [a] -> MarkovChain a
markovChain tokens = chain
  where
    chain = Map.mapWithKey toMarkovNode . nGramMap $ tokens
    toMarkovNode ngram = MarkovNode ngram . map (chain Map.!)

getToken :: MarkovNode a -> a 
getToken (MarkovNode ngram nodes) = fst ngram

getNext :: RandomGen g => MarkovNode a -> g -> (MarkovNode a, g)
getNext (MarkovNode ngram nodes) g = (nodes !! i, g')
  where
    (i, g') = randomR (0, length nodes - 1) g

generateTokens :: RandomGen g => MarkovNode a -> g -> [a]
generateTokens = go
  where
    go n g = n' `seq` (getToken n : go n' g')
      where
        (n', g') = getNext n g
