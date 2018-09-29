{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

{- |
Description : Mutable variables

This @State@ module provides the ability to 'Put' and 'Get' the value of multiple mutable variables in a @do@ block.

>>> :set -XTypeApplications
>>> :set -XRebindableSyntax
>>> import Prelude hiding ((>>), (>>=), return)
>>> import Control.Dsl
>>> import Data.Sequence
>>> :{
formatter :: Double -> Integer -> Seq String -> String
formatter = do
  --
  -- Equivalent of `!Put(!Get[Vector[Any]] :+ "x=")` in Dsl.scala
  tmpBuffer0 <- Get @(Seq String)
  Put $ tmpBuffer0 |> "x="
  --
  -- Equivalent of `!Put(!Get[Vector[Any]] :+ !Get[Double])` in Dsl.scala
  tmpBuffer1 <- Get @(Seq String)
  d <- Get @Double
  Put $ tmpBuffer1 |> show d
  --
  -- Equivalent of `!Put(!Get[Vector[Any]] :+ ",y=")` in Dsl.scala
  tmpBuffer2 <- Get @(Seq String)
  Put $ tmpBuffer2 |> ",y="
  --
  -- Equivalent of `!Put(!Get[Vector[Any]] :+ !Get[Int])` in Dsl.scala
  tmpBuffer3 <- Get @(Seq String)
  i <- Get @Integer
  Put $ tmpBuffer3 |>  show i
  --
  -- Equivalent of `!Return((!Get[Vector[Any]]).mkString)` in Dsl.scala
  tmpBuffer4 <- Get @(Seq String)
  return $ foldl1 (++) tmpBuffer4
:}

>>> formatter 0.5 42 Empty
"x=0.5,y=42"
-}
module Control.Dsl.State where

import Prelude hiding ((>>), (>>=), return)
import Control.Dsl.Internal

data Put a b where
  Put :: a -> Put a ()

instance Dsl (Put a) () (a -> b) where
  Put a >>= f = const $ f () a

data Get a = Get

instance Dsl Get a (a -> b) where
  (Get >>= f) a = f a a