import Control.Monad (replicateM)
import Control.Monad.Random (evalRandIO)
import Data.List (nub)
import Text.Printf (printf)
import System.Console.GetOpt  -- (getOpt, ArgOrder(Permute), usageInfo, NoArg, Option)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)

import Data.Markov (markovChain, MarkovChain)
import Data.Markov.Passphrase (cleanForPassphrase, passphrase)

data Flag
  = MinEntropy Double
  | Number Int
  | Suppress
  | Help
  deriving (Eq, Show)

flags = [
  Option
    ['m']
    ["min-entropy"]
    (ReqArg (MinEntropy . read) "MINENTROPY")
    "Minumum Entropy (defaults to 60)",
  Option
    ['n']
    ["number"]
    (ReqArg (Number . read) "NUMBER")
    "Number of passphrases to generate",
  Option
    ['s']
    ["suppress"]
    (NoArg Suppress)
    "Suppress entropy output",
  Option
    ['h']
    ["help"]
    (NoArg Help)
    "Print this help message"
  ]

parseArgs :: [String] -> IO (Double, Int, Bool, [String])
parseArgs argv = case getOpt Permute flags argv of
  (args, files, []) ->
      if Help `elem` args then hPutStrLn stderr usage >> exitSuccess
      else do
        minEntropy <- getValue 60 [e | x@(MinEntropy e) <- args]
        number <- getValue 1 [n | x@(Number n) <- args]
        let suppresEntropy = Suppress `elem` args
        return (minEntropy, number, suppresEntropy, files)
  (_, _, errs) -> hPutStrLn stderr (concat errs ++ usage) >> exitFailure
  where
    usage = usageInfo "Usage: passphrase [OPTION]... [FILE]..." flags
    getValue d [] = return d
    getValue _ [x] = return x
    getValue _ _ = do
      hPutStrLn stderr $ "Only one argument of each type allowed\n" ++ usage
      exitFailure

genPassphrase :: Double -> MarkovChain Char -> IO (String, Double)
genPassphrase minEntropy = evalRandIO . passphrase minEntropy

getCorpus :: [FilePath] -> IO String
getCorpus [] = getContents
getCorpus filenames = concat <$> mapM readFile filenames

main :: IO ()
main = do
  (minEntropy, number, suppressEntropy, files) <- getArgs >>= parseArgs
  corpus <- cleanForPassphrase <$> getCorpus files
  passphrases <- replicateM number . genPassphrase minEntropy . markovChain 3 $ corpus
  mapM_ (printPassphrase suppressEntropy) passphrases
  where
    printPassphrase True = printf "%s\n" . fst
    printPassphrase False = uncurry (printf "%s <%.2f>\n")
