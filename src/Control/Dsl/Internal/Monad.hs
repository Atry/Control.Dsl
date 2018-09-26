{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Dsl.Internal.Monad where

import Control.Dsl.Internal

instance Monad m => Dsl m a (m b) where
  (>>=) = (Prelude.>>=)
