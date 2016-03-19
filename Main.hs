import Control.Monad.Random (evalRandIO)
import Data.Markov (MarkovChain)
import Data.Markov.String (genSentence, fromString)

fromFiles :: [FilePath] -> IO (MarkovChain String)
fromFiles filenames = do
  s <- concat <$> (mapM readFile $ filenames)
  return $ fromString 2 s

main = do
  chain <- fromFiles [
      "corpus/lovecraft/mountains_of_madness.txt",
      "corpus/lovecraft/dunwich.txt"
    ]
  sentence <- evalRandIO $ genSentence chain
  print sentence
