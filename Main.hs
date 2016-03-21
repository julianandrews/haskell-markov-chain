import Control.Monad.Random (evalRandIO)
import Data.Markov (markovChain)
import Data.Markov.Passphrase (cleanForPassphrase, passphrase)

lovecraftFiles = [
    "corpus/mountains_of_madness.txt",
    "corpus/alchemist.txt",
    "corpus/dunwich.txt"
  ]

austenFiles = [
    "corpus/emma.txt",
    "corpus/pride.txt"
  ]

concatFiles :: [FilePath] -> IO String
concatFiles filenames = concat <$> mapM readFile filenames

printPassphrase :: [FilePath] -> IO ()
printPassphrase filenames = do
  chain <- markovChain 3 . cleanForPassphrase <$> concatFiles filenames
  pass <- evalRandIO $ passphrase 60 chain
  print pass

main :: IO ()
main = printPassphrase $ lovecraftFiles ++ austenFiles
