module Markov (markovChain, generateTokens, tokenize) where

import qualified Data.Map as Map
import Data.Random.Source.DevRandom (DevRandom(..))
import Data.Random.Extras (choice)
import Data.RVar (RVar, runRVar)

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

getNext :: MarkovNode a -> RVar (MarkovNode a)
getNext (MarkovNode ngram nodes) = choice nodes

generateTokens :: Int -> MarkovNode a -> RVar [a]
generateTokens 0 _ = return []
generateTokens n node = do
  next <- getNext node
  tail <- generateTokens (n - 1) next
  return $ getToken node : tail

tokenize :: String -> [String]
tokenize = words
