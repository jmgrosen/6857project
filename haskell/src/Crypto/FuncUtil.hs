module Crypto.FuncUtil
  ( powerAssoc
  ) where

powerAssoc :: (a -> a -> a) -> a -> a -> Integer -> a
powerAssoc  _ z _ 0 = z
powerAssoc op z x n =
  let x' = powerAssoc op z x (n `div` 2)
      x'' = op x' x'
  in if even n then x'' else op x x''
