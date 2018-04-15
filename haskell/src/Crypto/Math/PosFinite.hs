{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Crypto.Math.PosFinite
  ( PosFinite
  , getPosFinite
  , mulPosFinite
  , packPosFinite
  ) where

import Control.Monad ( guard )
import Data.Finite.Internal ( Finite(..) )
import Data.Finite ( getFinite, packFinite )
import Data.Proxy ( Proxy(..) )
import GHC.TypeLits ( Nat, KnownNat, natVal )
import System.Random ( Random(..) )

newtype PosFinite (n :: Nat) = PosFinite (Finite n)

packPosFinite :: KnownNat n => Integer -> Maybe (PosFinite n)
packPosFinite x = do
  guard (x /= 0)
  PosFinite <$> packFinite x

getPosFinite :: PosFinite n -> Integer
getPosFinite (PosFinite x) = getFinite x

instance Show (PosFinite n) where
  showsPrec d (PosFinite x) =
    showParen (d > 9) $ showString "posfinite " . showsPrec 10 (getFinite x)

instance forall n. (KnownNat n) => Random (PosFinite n) where
  randomR = error "randomR not yet defined for PosFinite"
  random gen = let (x, gen') = randomR (1, natVal (Proxy @n) - 1) gen
               in (PosFinite (Finite x), gen')

mulPosFinite :: KnownNat n => PosFinite n -> PosFinite n -> PosFinite n
mulPosFinite (PosFinite x) (PosFinite y) = PosFinite (x * y)
