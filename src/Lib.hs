module Lib (choice) where

import Control.Monad.Random (RandomGen, Rand, getRandomR)

choice :: RandomGen g => [a] -> Rand g a
choice elements = (elements !!) <$> getRandomR (0, length elements - 1)
