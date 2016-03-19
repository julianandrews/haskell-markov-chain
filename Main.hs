import Control.Monad (liftM)
import Data.Markov (MarkovChain)
import Data.Markov.String (genSentence, fromString)
import System.Random (getStdGen)

fromFiles :: Int -> [FilePath] -> IO (MarkovChain String)
fromFiles n filenames = do
  s <- liftM concat . mapM readFile $ filenames
  return $ fromString n s

main = do
  chain <- fromFiles 2 [
      "corpus/lovecraft/dunwich.txt",
      "corpus/lovecraft/mountains_of_madness.txt"
    ]
  g <- getStdGen
  print . fst . genSentence chain $ g
