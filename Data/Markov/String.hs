module Data.Markov.String (generateSentence, tokenize) where

import System.Random (RandomGen)

import Data.Markov

tokenize :: String -> [String]
tokenize = words

detokenize :: [String] -> String
detokenize = unwords

endsSentence word = last word `elem` ".!?"

dropSentence :: [String] -> [String]
dropSentence (word:words)
  | endsSentence word = words
  | otherwise = dropSentence words

takeSentence :: [String] -> [String]
takeSentence (word:words)
  | endsSentence word = [word]
  | otherwise = word : takeSentence words

generateSentence :: RandomGen g => MarkovChain String -> g -> String
generateSentence chain = getFirstFullSentence . getWords chain
  where
    getWords chain = uncurry generateTokens . getRandomNode chain
    getFirstFullSentence = detokenize . takeSentence . dropSentence
