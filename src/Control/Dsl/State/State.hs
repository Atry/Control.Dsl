{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Control.Dsl.State.State where

{- |
The type that holds states, which is defined as a plain function.

==== __Examples__

>>> :set -XFlexibleContexts
>>> :set -XTypeApplications
>>> :set -XRebindableSyntax
>>> import Prelude hiding ((>>), (>>=), return)
>>> import Control.Dsl
>>> import Control.Dsl.Cont
>>> import Control.Dsl.State
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
