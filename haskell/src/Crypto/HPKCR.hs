{-# LANGUAGE RankNTypes #-}

module Crypto.HPKCR where

import Control.Monad.Random.Class ( MonadRandom(getRandomR) )

data HPKCR pk sk p c = HPKCR
  { keyGen :: forall m. (MonadRandom m) => m (pk, sk)
  -- ^ Generate a key given randomness.
  , enc :: forall m. (MonadRandom m) => pk -> p -> m c
  -- ^ Encrypt a mesage with the given public key and randomness.
  , dec :: sk -> c -> p
  -- ^ Decrypt a message with the given secret key.
  , group :: pk -> pk -> pk
  -- ^ Perform the group operation on public keys.
  , invPubKey :: sk -> pk
  -- ^ Compute (pk(sk))^(-1).
  , addLayer :: c -> sk -> c
  -- ^ Given [m]_k, compute [m]_{k * pk(sk)}
  , delLayer :: c -> sk -> c
  -- ^ Given [m]_k, compute [m]_{k * pk(sk)^(-1)}
  , reRandomize :: forall m. (MonadRandom m) => c -> pk -> m c
  -- ^ Re-randomize such that the new ciphertext decrypts to the same as the first.
  , hMult :: c -> c -> c
  -- ^ Perform homomorphic "multiplication" on ciphertexts.
  , embedBit :: Bool -> p
  -- ^ Embed a bit into the plaintext representation.
  }

-- | Homomorphically raise a ciphertext to the given power.
hPower :: HPKCR pk sk p c -> c -> Integer -> c
hPower _ _ 0 = error "hPower not defined for 0"
hPower _ c 1 = c
hPower h c n =
  let c' = hPower h c (n `div` 2)
      c'' = hMult h c' c'
  in if even n then c'' else hMult h c c''

homOr :: (MonadRandom m) => HPKCR pk sk p c -> c -> c -> pk -> m c
homOr h c c' pk = do
  -- XXX: what random ranges do we want here?
  r <- getRandomR (1, undefined)
  let chat = hPower h c r
  r' <- getRandomR (1, undefined)
  let c'hat = hPower h c' r'
  reRandomize h (hMult h chat c'hat) pk
