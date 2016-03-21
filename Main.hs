import Control.Monad.Random (evalRandIO)
import Data.Markov (markovChain)
import Data.Markov.Passphrase (cleanForPassphrase, passphrase)

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
  pass <- evalRandIO $ passphrase minEntropy chain
  print pass

main :: IO ()
main = printPassphrase corpus 60
