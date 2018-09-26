{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Dsl.Yield where

import Control.Dsl.Internal

newtype Yield a = Yield a

instance Dsl Yield a [a] where
  (>>=) (Yield a) handler = a : handler a
