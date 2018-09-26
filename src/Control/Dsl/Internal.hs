{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Dsl.Internal where

class Dsl m a d where
  (>>=) :: m a -> (a -> d) -> d
  (>>) :: m a -> d -> d
  (>>) ma d = ma Control.Dsl.Internal.>>= \a -> d
