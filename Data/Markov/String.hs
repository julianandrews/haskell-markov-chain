module Data.Markov.String (genSentence, fromString) where

import Data.Markov
import System.Random (RandomGen)

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

genSentence :: RandomGen g => MarkovChain String -> g -> String
genSentence = (firstFullSentence .) . generateTokens

fromString :: Int -> String -> MarkovChain String
fromString n = markovChain n . words
