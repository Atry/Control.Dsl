{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}

{- |
Description : Generators

This module contains the 'Yield' data type and related 'Dsl' instances.

The 'Yield' data type can be used to create generators,
similar to the @yield@ keyword in C#, Python, and ECMAScript.
-}
module Control.Dsl.Yield where

import Control.Dsl.Cont
import Control.Dsl.PolyCont
import Prelude hiding ((>>), (>>=), return, fail)

{- | This @Yield@ keyword produces an element in a list generator

==== __Examples__

@randomGenerator@ is an example to create
an xorshift pseudo-random number generator
that returns an infinite list of generated numbers.

>>> :set -XTypeApplications
>>> :set -XRebindableSyntax
>>> import Prelude hiding ((>>), (>>=), return, fail)
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
data Yield x r a where
  Yield :: x -> Yield x r ()

instance PolyCont (Yield x) [x] () where
  runPolyCont (Yield x) f = x : f ()

instance PolyCont (Yield x) (Cont r [x]) () where
  runPolyCont (Yield x) f = Cont $ \g -> runCont (f ()) $ \xs -> g (x : xs)
