{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{- |
Description : Mutable variables

This module provides keywords to 'Put' and 'Get' the value of multiple mutable variables in a @do@ block.
-}
module Control.Dsl.State where

import Prelude hiding ((>>), (>>=), return)
import Control.Dsl.Dsl


{- |
The type that holds states, which is defined as a plain function.

==== __Examples__

>>> :set -XFlexibleContexts
>>> :set -XTypeApplications
>>> :set -XRebindableSyntax
>>> import Prelude hiding ((>>), (>>=), return)
>>> import Control.Dsl
>>> import Control.Dsl.Cont
>>> import Data.Sequence
>>> import Data.Foldable

The following @append@ function 'Get's a @Seq String@ state,
appends @s@ to the 'Seq', and 'Put's the new 'Seq' to the updated state.

>>> :{
append s = do
  buffer <- Get @(Seq String)
  Put $ buffer |> s
  ($ ())
:}

>>> :type append
append
  :: (Dsl (Put (Seq String)) b (), Dsl Get b (Seq String)) =>
     String -> (() -> b) -> b

A @formatter@ @append@s 'String' to its internal buffer,
and 'Control.Dsl.return' the concatenated buffer.

>>> :{
formatter = do
  Cont $ append "x="
  d <- Get @Double
  Cont $ append $ show d
  Cont $ append ",y="
  i <- Get @Integer
  Cont $ append $ show i
  buffer <- Get @(Seq String)
  return $ concat buffer
:}

>>> :type formatter
formatter
  :: (Dsl (Put (Seq String)) r (),
      Dsl (Control.Dsl.Return.Return [Char]) r Data.Void.Void,
      Dsl Get r (Seq String), Dsl Get r Double, Dsl Get r Integer) =>
     r

>>> x = 0.5 :: Double
>>> y = 42 :: Integer
>>> initialBuffer = Empty :: Seq String
>>> formatter x y initialBuffer :: String
"x=0.5,y=42"

Note that @formatter@ accepts arbitrary order of the parameters,
or additional unused parameters.

>>> formatter "unused parameter" initialBuffer y x :: String
"x=0.5,y=42"
-}
type State a b = a -> b

data Put a r u where
  Put :: a -> Put a r ()

instance Dsl (Put a) (State a b) () where
  cpsApply (Put a) f _ = f () a

data Get r a where
  Get :: forall a r. Get r a

instance Dsl Get (State a b) a where
  cpsApply Get f a = f a a
