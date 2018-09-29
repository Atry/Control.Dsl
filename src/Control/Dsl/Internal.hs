{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Dsl.Internal where

import Prelude ()

class Dsl m a d where
  (>>=) :: m a -> (a -> d) -> d
  (>>) :: m a -> d -> d
  ma >> d = ma >>= \a -> d

instance {-# OVERLAPPABLE #-} Dsl m a d => Dsl m a (b -> d) where
  (k >>= f) b = k >>= \a -> f a b
