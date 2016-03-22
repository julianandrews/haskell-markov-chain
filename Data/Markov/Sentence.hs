module Data.Markov.Sentence (sentence) where

import Control.Monad.Random (Rand, RandomGen)
import Data.Markov
import qualified Data.Map as Map

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

sentence :: RandomGen g => MarkovChain String -> Rand g String
sentence = (firstFullSentence <$>) . iterateTokens . choice . Map.elems
