{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Finite.RandomInstance where

import Data.Proxy ( Proxy(..) )
import GHC.TypeLits ( KnownNat, natVal )

import Data.Finite.Internal ( Finite(..) )
import System.Random ( Random(..) )

instance forall n. (KnownNat n) => Random (Finite n) where
  randomR = error "randomR not yet defined for Finite"
  random gen = let (x, gen') = randomR (0, natVal (Proxy @n) - 1) gen
               in (Finite x, gen')
