module Data.Markov.String (generateSentence, tokenize) where

import System.Random (RandomGen)

import Data.Markov

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
