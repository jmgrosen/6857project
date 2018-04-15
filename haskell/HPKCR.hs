module HPKCR where

data HPKCR pk sk m c = HPKCR
  { keyGen :: [Bool] -> (pk, sk)
  -- ^ Generate a key given randomness.
  , enc :: pk -> m -> [Bool] -> c
  -- ^ Encrypt a mesage with the given public key and randomness.
  , dec :: sk -> c -> m
  -- ^ Decrypt a message with the given secret key.
  , group :: pk -> pk -> pk
  -- ^ Perform the group operation on public keys.
  , invPubKey :: sk -> pk
  -- ^ Compute (pk(sk))^(-1).
  , addLayer :: c -> sk -> c
  -- ^ Given [m]_k, compute [m]_{k * pk(sk)}
  , delLayer :: c -> sk -> c
  -- ^ Given [m]_k, compute [m]_{k * pk(sk)^(-1)}
  , reRandomize :: c -> pk -> [Bool] -> c
  -- ^ Re-randomize such that the new ciphertext decrypts to the same as the first.
  , hMult :: c -> c -> c
  -- ^ Perform homomorphic "multiplication" on ciphertexts.
  }

-- | Homomorphically raise a ciphertext to the given power.
hPower :: HPKCR pk sk m c -> c -> Integer -> c
hPower = undefined

homOr :: HPKCR pk sk m c -> c -> c -> pk -> (Integer, Integer) -> [Bool] -> c
homOr h c c' pk (r, r') r'' =
  let chat = hPower h c r
      c'hat = hPower h c' r'
  in reRandomize h (hMult h chat c'hat) pk r''
