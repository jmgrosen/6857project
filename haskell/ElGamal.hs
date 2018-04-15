module ElGamal where

data CyclicGroup a q = CyclicGroup
  { cgOp :: a -> a -> a
  , cgId :: a
  , cgGen :: a
  }

cgPower :: CyclicGroup a q -> a -> Integer -> a
cgPower cg _ 0 = cgId cg
cgPower  _ g 1 = g
cgPower cg g n =
  let g' = cgPower cg g (n `div` 2)
      g'' = cgOp cg g' g'
  in if n `mod` 2 == 1 then cgOp cg g g'' else g''
