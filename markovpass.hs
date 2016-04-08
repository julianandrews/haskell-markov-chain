import Control.Monad (replicateM)
import Control.Monad.Random (evalRandIO)
import Text.Read (readMaybe)
import Text.Printf (printf)
import System.Console.GetOpt (getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..), usageInfo)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)

import Data.Markov (markovChain)
import Data.Markov.Passphrase (cleanCorpus, passphrase)

data Flag
  = MinEntropy Double
  | Number Int
  | NGramLength Int
  | MinWordLength Int
  | Suppress
  | Help
  deriving (Eq, Show)

flags :: [OptDescr Flag]
flags = [
  Option
    ['m'] ["minentropy"] (ReqArg (MinEntropy . read) "MINENTROPY")
    "Minumum Entropy (default 60)",
  Option
    ['n'] ["number"] (ReqArg (Number . read) "NUMBER")
    "Number of passphrases to generate (default 1)",
  Option
    ['l'] ["length"] (ReqArg (NGramLength . read) "LENGTH")
    "NGram Length (default 3)",
  Option
    ['w'] ["minwordlength"] (ReqArg (MinWordLength . read) "LENGTH")
    "Minimum word length for corpus (default 5)",
  Option
    ['s'] ["suppress"] (NoArg Suppress)
    "Suppress work output",
  Option
    ['h'] ["help"] (NoArg Help)
    "Print this help message"
  ]

usage :: String
usage = usageInfo "Usage: passphrase [OPTION]... [FILE]..." flags

printError :: String -> IO a
printError message = hPutStrLn stderr (message ++ '\n': usage) >> exitFailure

parseArgs :: [String] -> IO (Double, Int, Int, Int, Bool, [String])
parseArgs argv = case getOpt Permute flags argv of
  (args, fs, []) ->
      if Help `elem` args then hPutStrLn stderr usage >> exitSuccess
      else do
        e <- getValue 60 [x | MinEntropy x <- args]
        n <- getValue 1 [x | Number x <- args]
        l <- getValue 3 [x | NGramLength x <- args]
        w <- getValue 5 [x | MinWordLength x <- args]
        let s = Suppress `elem` args
        return (e, n, l, w, s, fs)
  (_, _, errs) -> printError (concat errs ++ usage)
  where
    getValue d [] = return d
    getValue _ [x]
      | x <= 0 = printError "Values must be greater than 0"
      | otherwise = return x
    getValue _ _ = printError "Only one argument allowed per flag."

getCorpus :: Int -> [FilePath] -> IO String
getCorpus w fs = do
  c <- cleanCorpus w <$> getInput fs
  if c == " " then printError "Empty corpus. Maybe try a shorter minimum word length?"
  else return c
  where
    getInput [] = getContents
    getInput filenames = concat <$> mapM readFile filenames

main :: IO ()
main = do
  (e, n, l, w, s, fs) <- getArgs >>= parseArgs
  c <- getCorpus w fs
  passphrases <- replicateM n . evalRandIO . passphrase e $! markovChain l c
  mapM_ (printPassphrase s) passphrases
  where
    printPassphrase True = printf "%s\n" . fst
    printPassphrase False = uncurry (printf "%s <%.2f>\n")
