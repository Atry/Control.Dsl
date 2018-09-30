{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}

module Control.Dsl.Internal.Dsl where

import Prelude ()

class Dsl m a d where
  (>>=) :: m a -> (a -> d) -> d
  (>>) :: m a -> d -> d
  ma >> d = ma >>= \a -> d
