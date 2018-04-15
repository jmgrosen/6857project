import Control.Monad.Random.Class ( MonadRandom(getRandomR) )

module Control.Monad.Random.Bounded where

class MonadRandomBounded m where
  getRandomBounded :: Integer -> m Integer

instance (MonadRandom m) => MonadRandomBounded m where
  getRandomBounded n = getRandomR (0, n - 1)
