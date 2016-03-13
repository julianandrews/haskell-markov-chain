import System.Random (getStdGen)

import Data.Markov.String (genSentence, fromFiles)

main = do
  chain <- fromFiles 2 [
      "corpus/lovecraft/dunwich.txt",
      "corpus/lovecraft/mountains_of_madness.txt"
    ]
  g <- getStdGen
  print $ genSentence chain g
