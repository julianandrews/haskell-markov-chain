module Data.Markov.String (genSentence, fromString) where

import Control.Monad.Random (Rand, RandomGen)
import Data.Markov

endsSentence :: String -> Bool
endsSentence word = last word `elem` ".!?"

dropSentence :: [String] -> [String]
dropSentence (word:words)
  | endsSentence word = words
  | otherwise = dropSentence words

takeSentence :: [String] -> [String]
takeSentence (word:words)
  | endsSentence word = [word]
  | otherwise = word : takeSentence words

firstFullSentence :: [String] -> String
firstFullSentence = unwords . takeSentence . dropSentence

fromString :: Int -> String -> MarkovChain String
fromString n = markovChain n . words

genSentence :: RandomGen g => MarkovChain String -> Rand g String
genSentence = (firstFullSentence <$>) . generateTokens
