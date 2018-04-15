{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Main ( main ) where

import Data.Proxy ( Proxy(..) )
import GHC.TypeLits ( SomeNat(..), someNatVal )

import Control.Monad.Random.Class ( MonadRandom(getRandom) )

import Crypto.HPKCR ( HPKCR(..) )
import Crypto.PubKey.ElGamal ( elGamal, ElGamalPT(..) )
import Crypto.Math.Prime ( millerRabin, genPrime )
import Crypto.Math.CyclicGroup ( intMulMod2Qp1 )

genSafePrime :: (MonadRandom m) => Integer -> m (Integer, Integer)
genSafePrime nbits = do
  q <- genPrime (millerRabin 4) (nbits - 1)
  let p = 2*q + 1
  b <- millerRabin 4 p
  if b
    then return (q, p)
    else genSafePrime nbits

main :: IO ()
main = do
  (q, p) <- genSafePrime 200
  putStr "q is "
  print q
  putStr "so p = 2*q + 1 is "
  print p
  Just (SomeNat (Proxy :: Proxy q)) <- return (someNatVal q)
  cg <- intMulMod2Qp1 @q
  let eg = elGamal cg
  (pk, sk) <- keyGen eg
  m <- ElGamalPT <$> getRandom
  putStr "the plaintext is "
  print m
  c <- enc eg pk m
  putStr "the ciphertext is "
  print c
  let m' = dec eg sk c
  putStr "the decrypted ciphertext is "
  print m'
