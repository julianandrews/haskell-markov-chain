module Markov (markovChain, generateTokens) where

import qualified Data.Map as Map
import Data.List (tails)
import System.Random (RandomGen, randomR, getStdGen)

type NGram a = [a]
data MarkovNode a = MarkovNode (NGram a) [MarkovNode a]
type MarkovChain a = Map.Map (NGram a) (MarkovNode a)

nGramMap :: Ord a => Int -> [a] -> Map.Map (NGram a) [NGram a]
nGramMap n = foldl addTransition Map.empty . toPairs . toNGrams n
  where
    addTransition m (from, to) = Map.insertWith (++) from [to] m
    toNGrams n = map (take n) . takeWhile ((>=n) . length) . tails
    toPairs l = zip l $ tail l

markovChain :: (Eq a, Ord a) => Int -> [a] -> MarkovChain a
markovChain n tokens = chain
  where
    chain = Map.mapWithKey toMarkovNode . nGramMap n $ tokens
    toMarkovNode ngram = MarkovNode ngram . map (chain Map.!)

getRandomNode :: RandomGen g => MarkovChain a -> g -> (MarkovNode a, g)
getRandomNode chain g = (nodes !! i, g')
  where
    (i, g') = randomR (0, length nodes - 1) g
    nodes = Map.elems chain

getToken :: MarkovNode a -> a 
getToken (MarkovNode ngram nodes) = head ngram

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

tokenize :: String -> [String]
tokenize = words

detokenize :: [String] -> String
detokenize = unwords

getSentenceStart :: RandomGen g => MarkovNode String -> g -> (MarkovNode String, g)
getSentenceStart node
  | last (getToken node) `elem` ".!?" = getNext node
  | otherwise = uncurry getSentenceStart . getNext node

generateSentence :: RandomGen g => MarkovChain String -> g -> String
generateSentence chain = detokenize . take 100 . uncurry generateTokens . getStartingNode chain
  where
    getStartingNode chain = uncurry getSentenceStart . getRandomNode chain

main = do
  s <- readFile "corpus/lovecraft/dunwich.txt"
  g <- getStdGen
  print $ generateSentence (markovChain 2 $ tokenize s) g
