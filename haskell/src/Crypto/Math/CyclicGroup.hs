{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Crypto.Math.CyclicGroup
  ( CyclicGroup(..)
  , cgPower'
  , cgPower
  , intAddModN
  , intMulMod2Qp1
  ) where

import Data.Maybe ( fromJust )
import Data.Proxy ( Proxy(..) )
import GHC.TypeLits ( KnownNat, Nat, natVal, type (*), type (+) )

import Control.Monad.Random.Class ( MonadRandom(getRandomR) )
import Data.Finite ( Finite, getFinite )
import Crypto.Math.PosFinite ( PosFinite, getPosFinite, mulPosFinite, packPosFinite )
import Crypto.Math.Prime ( modExp )

data CyclicGroup a (q :: Nat) = CyclicGroup
  { cgOp :: a -> a -> a
  , cgId :: a
  , cgInv :: a -> a
  , cgGen :: a
  }

cgPower' :: CyclicGroup a q -> a -> Integer -> a
cgPower' cg _ 0 = cgId cg
cgPower'  _ g 1 = g
cgPower' cg g n =
  let g' = cgPower' cg g (n `div` 2)
      g'' = cgOp cg g' g'
  in if even n then g'' else cgOp cg g g''

cgPower :: CyclicGroup a q -> a -> Finite q -> a
cgPower cg g x = cgPower' cg g (getFinite x)

intAddModN :: (KnownNat n) => CyclicGroup (Finite n) n
intAddModN = CyclicGroup
  { cgOp = (+)
  , cgId = 0
  , cgInv = negate
  , cgGen = 1
  }

egcd :: (Integral a) => a -> a -> (a, a, a)
egcd 0 b = (b, 0, 1)
egcd a b =
    let (d, x, y) = egcd (b `rem` a) a
    in (d, y - x * (b `div` a), x)

findGenerator :: (MonadRandom m) => Integer -> m Integer
findGenerator q = do
  g <- getRandomR (3, 2 * q)
  if modExp g 2 (2*q + 1) /= 1 && modExp g q (2*q + 1) /= 1
    then return g
    else findGenerator q

-- | The integer multiplication mod (2*q + 1) group, where 2*q+1 is a safe
-- prime.
intMulMod2Qp1 :: forall q m. (KnownNat q, MonadRandom m) => m (CyclicGroup (PosFinite (2 * q + 1)) (2 * q))
intMulMod2Qp1 = do
  let qVal = natVal (Proxy @q)
      pVal = natVal (Proxy @(2 * q + 1))
  g <- findGenerator qVal
  return $ CyclicGroup
    { cgOp = mulPosFinite
    , cgId = fromJust (packPosFinite 1)
    , cgInv = \a -> let (d, x, _) = egcd (getPosFinite a) pVal
                        x' = x `mod` pVal
                    in if d == 1
                       then fromJust (packPosFinite x')
                       else error "d /= 1? wat?"
    , cgGen = fromJust (packPosFinite g)
    }
