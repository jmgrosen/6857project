module Crypto.Math.Prime
  ( millerRabin
  , modExp
  , genPrime
  ) where

import Data.List ( findIndices )
import Data.Maybe ( fromJust )

import Control.Monad.Random.Class ( MonadRandom(getRandomR) )

maxPow2 :: Integer -> Int
maxPow2 n = head $ findIndices test [(1 :: Integer)..]
  where test i = n `rem` (2 ^ i) /= 0

modExp :: Integer -> Integer -> Integer -> Integer
modExp _ 0 _ = 1
modExp a 1 _ = a
modExp a b n =
  let a' = modExp a (b `div` 2) n
      a'' = (a' * a') `mod` n
  in if b `mod` 2 == 1 then (a * a'') `mod` n else a''

millerRabinInner :: Integer -> Integer -> Integer -> Bool
millerRabinInner _ _ 0 = False
millerRabinInner x n m =
  let x' = (x * x) `mod` n
  in if x' == 1
     then False
     else if x' == n - 1
          then True
          else millerRabinInner x' n (m - 1)

millerRabin' :: (MonadRandom m) => Integer -> Integer -> Integer -> m Bool
millerRabin' n r d = do
  a <- getRandomR (2, n - 2)
  let x = modExp a d n
  if x == 1 || x == n - 1
    then return True
    else return (millerRabinInner x n (r - 1))

allM :: (Monad m) => [m Bool] -> m Bool
allM [] = return True
allM (x : xs) = do
  b <- x
  if b then allM xs else return False

millerRabin :: (MonadRandom m) => Int -> Integer -> m Bool
millerRabin _ 1 = return False
millerRabin _ 2 = return True
millerRabin _ 3 = return True
millerRabin k n
  | even n    = return False
  | otherwise =
    let r = toInteger (maxPow2 (n - 1))
        d = n `div` (2 ^ r)
    in allM $ take k $ repeat (millerRabin' n r d)

firstM :: (Monad m) => (a -> m Bool) -> [m a] -> m (Maybe a)
firstM _ [] = return Nothing
firstM f (x : xs) = do
  x' <- x
  b <- f x'
  if b
    then return (Just x')
    else firstM f xs

genPrime :: (MonadRandom m) => (Integer -> m Bool) -> Integer -> m Integer
genPrime test nbits = fromJust <$> firstM test (repeat r)
  where r = ((2 ^ nbits) +) <$> getRandomR (1, 2 ^ (nbits - 1))
