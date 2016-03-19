import Control.Monad.Random (evalRandIO)
import Data.Markov (MarkovChain, markovChain)
import Data.Markov.String (sentence, fromString)
import Data.Markov.Entropy

import Data.Char (toLower, isAlpha)

concatFiles :: [FilePath] -> IO String
concatFiles filenames = concat <$> mapM readFile filenames

printSentence :: [FilePath] -> IO ()
printSentence filenames = do
  chain <- fromString 2 words <$> concatFiles filenames
  sentence <- evalRandIO $ sentence chain
  print sentence

lovecraftFiles = [
    "corpus/mountains_of_madness.txt",
    "corpus/alchemist.txt",
    "corpus/dunwich.txt"
  ]

austenFiles = [
    "corpus/emma.txt",
    "corpus/pride.txt"
  ]

main :: IO ()
main = printSentence lovecraftFiles

-- cleanString :: String -> String
-- cleanString = unwords . filter isGood . map cleanWord . words
--   where
--     cleanWord = map toLower . takeWhile isAlpha . dropWhile (not . isAlpha)
--     isGood = (>6) . length

-- printWordsWithEntropy :: [FilePath] -> IO ()
-- printWordsWithEntropy filenames = do
--   chain <- fromString 3 id . cleanString <$> concatFiles filenames
--   foo <- evalRandIO $ take 30 <$> tokensWithEntropy chain
--   -- The last node's entropy isn't used
--   let entropy = chainEntropy chain + (sum . map snd . init $ foo)
--   let words = map fst foo
--   print (words, entropy)

-- main :: IO ()
-- main = printWordsWithEntropy $ lovecraftFiles ++ austenFiles
