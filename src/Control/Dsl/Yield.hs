{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}

{- |
Description : Generators

This module contains the 'Yield' data type and related 'Dsl' instances.

The 'Yield' data type can be used to create generators,
similar to the @yield@ keyword in C#, Python, and ECMAScript.

=== Examples

@randomGenerator@ is an example to create
an xorshift pseudo-random number generator
that returns an infinite list of generated numbers.

>>> :set -XTypeApplications
>>> :set -XRebindableSyntax
>>> import Prelude hiding ((>>), (>>=), return)
>>> import Control.Dsl
>>> import Data.Word
>>> import Data.Bits
>>> :{
randomGenerator :: Word32 -> [Word32]
randomGenerator seed =
  do let tmp1 = xor seed $ shiftL seed 13
     let tmp2 = xor tmp1 $ shiftR tmp1 17
     let tmp3 = xor tmp2 $ shiftL tmp2 5
     Yield tmp3
     randomGenerator tmp3
:}

>>> take 5 $ randomGenerator 2463534242
[723471715,2497366906,2064144800,2008045182,3532304609]
-}
module Control.Dsl.Yield where

import Control.Dsl.Internal.Dsl

data Yield a b where
  Yield :: a -> Yield a ()

instance Dsl (Yield a) () [a] where
  Yield a >>= f = a : f ()
