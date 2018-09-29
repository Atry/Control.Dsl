{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Dsl.Internal.Monad where

import Control.Dsl.Internal
import qualified Prelude

instance Prelude.Monad m => Dsl m a (m b) where
  (>>=) = (Prelude.>>=)
