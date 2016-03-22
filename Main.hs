import Control.Monad.Random (evalRandIO)

import Data.Markov (markovChain)
import Data.Markov.Passphrase (cleanForPassphrase, passphrase)
-- import Data.Markov.Sentence (sentence)

corpus = [
    "corpus/mountains_of_madness.txt",
    "corpus/alchemist.txt",
    "corpus/dunwich.txt",
    "corpus/emma.txt",
    "corpus/pride.txt"
  ]

concatFiles :: [FilePath] -> IO String
concatFiles filenames = concat <$> mapM readFile filenames

printPassphrase :: [FilePath] -> Double -> IO ()
printPassphrase filenames minEntropy = do
  chain <- markovChain 3 . cleanForPassphrase <$> concatFiles filenames
  result <- evalRandIO $ passphrase minEntropy chain
  print result

-- printSentece :: [FilePath] -> IO ()
-- printSentence filenames = do
--   chain <- markovChain 2 . words <$> concatFiles filenames
--   result <- evalRandIO $ sentence chain
--   print result

main :: IO ()
main = printPassphrase corpus 60
-- main = printSentence corpus
