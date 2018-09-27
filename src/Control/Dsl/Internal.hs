{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Dsl.Internal where

class Dsl m a d where
  (>>=) :: m a -> (a -> d) -> d
  (>>) :: m a -> d -> d
  (>>) ma d = ma Control.Dsl.Internal.>>= \a -> d

instance {-# OVERLAPPABLE #-} Dsl m a d => Dsl m a (b -> d) where
  (>>=) k handler state = k Control.Dsl.Internal.>>= \x -> handler x state
