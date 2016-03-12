import qualified Data.Map as Map

type Token = String
type MarkovChain = Map.Map NGram MarkovNode
type NGram = (Token, Token)

data MarkovNode = MarkovNode NGram [MarkovNode]

tokenize :: String -> [Token]
tokenize = words

addTransition :: MarkovChain -> (NGram, NGram) -> MarkovChain
addTransition m _ = m

generateChain :: String -> MarkovChain
generateChain = foldl addTransition Map.empty . toPairs . toNGrams . tokenize
  where
    toNGrams = toPairs
    toPairs l = zip l (tail $ cycle l)
