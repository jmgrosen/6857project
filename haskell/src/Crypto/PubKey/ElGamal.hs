{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crypto.PubKey.ElGamal
  ( CyclicGroup(..)
  , cgPower
  , elGamal
  , ElGamalPK(..)
  , ElGamalSK(..)
  , ElGamalPT(..)
  , ElGamalCT(..)
  ) where

import GHC.TypeLits ( KnownNat )

import Data.Finite ( Finite )
import Data.Finite.RandomInstance ()

import Control.Monad.Random.Class ( getRandom )

import Crypto.HPKCR
import Crypto.Math.CyclicGroup

data ElGamalPK a = ElGamalPK a deriving (Show)
data ElGamalSK q = ElGamalSK (Finite q) deriving (Show)
data ElGamalPT a = ElGamalPT a deriving (Show)
data ElGamalCT a = ElGamalCT a a deriving (Show)

elGamal :: forall a q.
           (KnownNat q)
        => CyclicGroup a q
        -> HPKCR (ElGamalPK a)
                 (ElGamalSK q)
                 (ElGamalPT a)
                 (ElGamalCT a)
elGamal cg = HPKCR
  { keyGen = do
      x :: Finite q <- getRandom
      return (ElGamalPK (cgPower cg (cgGen cg) x), ElGamalSK x)
  , enc = \(ElGamalPK y) (ElGamalPT m) -> do
      k :: Finite q <- getRandom
      return (ElGamalCT (cgPower cg (cgGen cg) k) (cgOp cg m (cgPower cg y k)))
  , dec = \(ElGamalSK x) (ElGamalCT a b) ->
      ElGamalPT (cgOp cg b (cgInv cg (cgPower cg a x)))
  , group = \(ElGamalPK y1) (ElGamalPK y2) ->
      ElGamalPK (cgOp cg y1 y2)
  , invPubKey = \(ElGamalSK x) ->
      ElGamalPK (cgInv cg (cgPower cg (cgGen cg) x))
  , addLayer = \(ElGamalCT a b) (ElGamalSK x) ->
      ElGamalCT a (cgOp cg b (cgPower cg (cgGen cg) x))
  , delLayer = \(ElGamalCT a b) (ElGamalSK x) ->
      ElGamalCT a (cgOp cg b (cgInv cg (cgPower cg (cgGen cg) x)))
  , reRandomize = \(ElGamalCT a b) (ElGamalPK y) -> do
      r :: Finite q <- getRandom
      return (ElGamalCT (cgOp cg a (cgPower cg (cgGen cg) r)) (cgOp cg (cgPower cg y r) b))
  , hMult = \(ElGamalCT a1 b1) (ElGamalCT a2 b2) ->
      ElGamalCT (cgOp cg a1 a2) (cgOp cg b1 b2)
  , embedBit = \b ->
      ElGamalPT (if b then cgGen cg else cgId cg)
  }
