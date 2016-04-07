import Control.Monad (replicateM, when)
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
  | NGramLength Int
  | MinWordLength Int
  | Suppress
  | Help
  deriving (Eq, Show)

flags = [
  Option
    ['m']
    ["minentropy"]
    (ReqArg (MinEntropy . read) "MINENTROPY")
    "Minumum Entropy (default 60)",
  Option
    ['n']
    ["number"]
    (ReqArg (Number . read) "NUMBER")
    "Number of passphrases to generate (default 1)",
  Option
    ['l']
    ["length"]
    (ReqArg (NGramLength .read) "LENGTH")
    "NGram Length (default 3)",
  Option
    ['w']
    ["minwordlength"]
    (ReqArg (MinWordLength . read) "LENGTH")
    "Minimum word length for corpus (default 5)",
  Option
    ['s']
    ["suppress"]
    (NoArg Suppress)
    "Suppress work output",
  Option
    ['h']
    ["help"]
    (NoArg Help)
    "Print this help message"
  ]

usage = usageInfo "Usage: passphrase [OPTION]... [FILE]..." flags

parseArgs :: [String] -> IO (Double, Int, Int, Int, Bool, [String])
parseArgs argv = case getOpt Permute flags argv of
  (args, fs, []) ->
      if Help `elem` args then hPutStrLn stderr usage >> exitSuccess
      else do
        e <- getValue 60 [x | MinEntropy x <- args]
        n <- getValue 1 [x | Number x <- args]
        l <- getValue 3 [x | NGramLength x <- args]
        w <- getValue 4 [x | MinWordLength x <- args]
        let s = Suppress `elem` args
        return (e, n, l, w, s, fs)
  (_, _, errs) -> hPutStrLn stderr (concat errs ++ usage) >> exitFailure
  where
    getValue d [] = return d
    getValue _ [x] = return x
    getValue _ _ = do
      hPutStrLn stderr $ "Only one argument allowed per flag\n" ++ usage
      exitFailure

genPassphrase :: Double -> MarkovChain Char -> IO (String, Double)
genPassphrase e = evalRandIO . passphrase e

getCorpus :: Int -> [FilePath] -> IO String
getCorpus w fs = do
  c <- cleanForPassphrase w <$> getInput fs
  if c == " " then do
    hPutStrLn stderr (
      "Empty corpus. Maybe try a shorter minimum word length?\n" ++ usage
      )
    exitFailure
  else
    return c
  where
    getInput [] = getContents
    getInput filenames = concat <$> mapM readFile filenames

main :: IO ()
main = do
  (e, n, l, w, s, fs) <- getArgs >>= parseArgs
  c <- getCorpus w fs
  passphrases <- replicateM n . genPassphrase e $! markovChain l c
  mapM_ (printPassphrase s) passphrases
  where
    printPassphrase True = printf "%s\n" . fst
    printPassphrase False = uncurry (printf "%s <%.2f>\n")


