module Data.Markov.String (genSentence, fromString, fromFile, fromFiles) where

import Control.Monad (liftM)
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

genSentence :: RandomGen g => MarkovChain String -> g -> String
genSentence chain = getFirstFullSentence . getWords chain
  where
    getWords chain = uncurry generateTokens . getRandomNode chain
    getFirstFullSentence = detokenize . takeSentence . dropSentence

fromString :: Int -> String -> MarkovChain String
fromString n = markovChain n . tokenize

fromFile :: Int -> FilePath -> IO (MarkovChain String)
fromFile n filename = do
  s <- readFile filename
  return $ fromString n s

fromFiles :: Int -> [FilePath] -> IO (MarkovChain String)
fromFiles n filenames = do
  s <- liftM concat . mapM readFile $ filenames
  return $ fromString n s
