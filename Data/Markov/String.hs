module Data.Markov.String (genSentence, fromString) where

import Control.Arrow (first)
import System.Random (RandomGen)

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

genSentence :: RandomGen g => MarkovChain String -> g -> (String, g)
genSentence chain = first getFirstFullSentence . getWords chain
  where
    getWords chain = uncurry generateTokens . getRandomNode chain
    getFirstFullSentence = unwords . takeSentence . dropSentence

fromString :: Int -> String -> MarkovChain String
fromString n = markovChain n . words
