import System.Random (getStdGen)

import Data.Markov (markovChain)
import Data.Markov.String (generateSentence, tokenize)

main = do
  s <- readFile "corpus/lovecraft/dunwich.txt"
  g <- getStdGen
  print $ generateSentence (markovChain 2 $ tokenize s) g
