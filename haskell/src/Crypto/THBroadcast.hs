{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Crypto.THBroadcast
  ( BroadcastSetup
  , initBroadcast
  , firstRound
  , forwardRound
  , middleRound
  , backwardRound
  , example
  ) where

import Control.Monad ( forM_ )
import Control.Monad.Random.Class ( MonadRandom(getRandom) )
import Control.Monad.ST ( runST )
import Data.Finite ( Finite, shift )
import Data.Maybe ( fromJust )
import Data.Proxy ( Proxy(Proxy) )
import Data.STRef ( newSTRef, readSTRef, writeSTRef )
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Sized as VS
import GHC.TypeLits ( Nat, KnownNat, natVal, type (+), type (*), type (-), type (<=), type (^) )
import System.Random ( Random(..) )

import Crypto.HPKCR ( HPKCR(..), homOr )

newtype Permutation (n :: Nat) = Permutation (VS.Vector n (Finite n))

evalPerm :: (KnownNat n) => Permutation n -> Finite n -> Finite n
evalPerm (Permutation v) = VS.index v

evalInvPerm :: (KnownNat n) => Permutation n -> Finite n -> Finite n
evalInvPerm (Permutation v) i = fromJust $ VS.elemIndex i v

instance forall n. (KnownNat n) => Random (Permutation n) where
  randomR = error "getRandomR is not defined for Permutation"
  random gen = (Permutation (fromJust (VS.toSized perm)), gen')
    where (perm, gen') = runST $ do
            genRef <- newSTRef gen
            v <- VM.new nVal
            forM_ [nVal-1..1] $ \i -> do
              g <- readSTRef genRef
              let (j, g') = randomR (0, i) g
              writeSTRef genRef g'
              VM.swap v i j
            g <- readSTRef genRef
            vImm <- V.freeze v
            return (vImm, g)
          nVal = fromInteger (natVal (Proxy @n))

type T n kappa = 8 * kappa * n^3

data BroadcastSetup pk sk n kappa di = BroadcastSetup
  { keyPairs :: VS.Vector (T n kappa) (VS.Vector di (pk, sk))
  , permutations :: VS.Vector (T n kappa - 1) (Permutation di)
  , bit :: Bool
  }

initBroadcast :: forall n kappa di pk sk p c m. (MonadRandom m, KnownNat n, KnownNat kappa, KnownNat di, 1 <= ((n^3) * (kappa * 8))) => HPKCR pk sk p c -> Bool -> m (BroadcastSetup pk sk n kappa di)
initBroadcast h bi =
  BroadcastSetup <$> VS.replicateM (VS.replicateM (keyGen h))
                 <*> VS.replicateM getRandom
                 <*> pure bi

firstRound :: (MonadRandom m,
               KnownNat n,
               KnownNat di,
               KnownNat kappa)
           => HPKCR pk sk p c
           -> BroadcastSetup pk sk n kappa di
           -> m (VS.Vector di (c, pk))
firstRound h bs =
  let keys = VS.index (keyPairs bs) 0
  in VS.generateM $ \d -> do
    let (pk, _) = VS.index keys d
    biEnc <- enc h pk (embedBit h (bit bs))
    return (biEnc, pk)

forwardRound :: (MonadRandom m,
                 KnownNat n,
                 KnownNat kappa,
                 KnownNat di,
                 1 <= ((n^3) * (kappa * 8)),
                 1 <= n,
                 1 <= kappa)
             => HPKCR pk sk p c
             -> BroadcastSetup pk sk n kappa di
             -> Finite (T n kappa - 1)
             -> VS.Vector di (c, pk)
             -> m (VS.Vector di (c, pk))
forwardRound h bs t neighbors =
  let perm = VS.index (permutations bs) t
      keys = VS.index (keyPairs bs) (shift t)
  in VS.generateM $ \d' -> do
    let d = evalInvPerm perm d'
        (c, kt) = VS.index neighbors d
        (pk, sk) = VS.index keys d'
        ktp1 = group h kt pk
        chat = addLayer h c sk
    biEnc <- enc h ktp1 (embedBit h (bit bs))
    ctp1 <- homOr h chat biEnc ktp1
    return (ctp1, ktp1)

middleRound :: (MonadRandom m,
                KnownNat n,
                KnownNat di,
                KnownNat kappa,
                1 <= ((n ^ 3) * (kappa * 8)))
            => HPKCR pk sk p c
            -> BroadcastSetup pk sk n kappa di
            -> VS.Vector di (c, pk)
            -> m (VS.Vector di c)
middleRound h bs neighbors =
  let perm = VS.index (permutations bs) maxBound
  in VS.generateM $ \d' -> do
    let d = evalPerm perm d'
        (c, kt) = VS.index neighbors d
    biEnc <- enc h kt (embedBit h (bit bs))
    homOr h c biEnc kt

backwardRound :: (KnownNat n,
                  KnownNat di,
                  KnownNat kappa,
                  1 <= ((n ^ 3) * (kappa * 8)))
              => HPKCR pk sk p c
              -> BroadcastSetup pk sk n kappa di
              -> Finite (T n kappa - 1)
              -> VS.Vector di c
              -> VS.Vector di c
backwardRound h bs t neighbors =
  let perm = VS.index (permutations bs) t
      keys = VS.index (keyPairs bs) (shift t) -- XXX
      -- keys = VS.index (keyPairs bs) (extend t) -- XXX
  in VS.generate $ \d' ->
    let d = evalPerm perm d'
        et = VS.index neighbors d
        (_, sk) = VS.index keys d
    in delLayer h et sk

-- type Graph n = VS.Vector n [Finite n]

data Graph' pk sk n kappa n' where
  GNil :: Graph' pk sk n kappa 0
  GCons :: BroadcastSetup pk sk n kappa di
        -> VS.Vector di (Finite n)
        -> Graph' pk sk n kappa n'
        -> Graph' pk sk n kappa (n' + 1)

type Graph pk sk n kappa = Graph' pk sk n kappa n

forwardRounds :: HPKCR pk sk p c
              -> Graph pk sk n kappa
              ->

example :: forall pk sk p c. HPKCR pk sk p c -> IO ()
example h = do
  zero <- initBroadcast @3 @2 @2 h False
  one <- initBroadcast @3 @2 @2 h False
  two <- initBroadcast @3 @2 @2 h False
  let graph :: Graph pk sk 3 2 = GCons zero (VS.fromTuple (1, 2)) $
                                 GCons one (VS.fromTuple (0, 2)) $
                                 GCons two (VS.fromTuple (0, 1)) $
                                 GNil
  return ()
