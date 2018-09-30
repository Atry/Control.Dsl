{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}

module Control.Dsl.Internal.Monad where

import Control.Dsl.Internal.Dsl
import qualified Prelude

instance Prelude.Monad m => Dsl m a (m b) where
  (>>=) = (Prelude.>>=)
