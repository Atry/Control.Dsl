{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}

module Control.Dsl.Monadic where

import Control.Dsl.Dsl
import qualified Prelude

newtype Monadic m r a = Monadic (m a)

instance Prelude.Monad m => Dsl (Monadic m) (m b) a where
  cpsApply (Monadic k) = (Prelude.>>=) k
