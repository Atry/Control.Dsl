{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}

module Control.Dsl.Monadic where

import Control.Dsl.PolyCont
import qualified Prelude

-- | This @Monadic@ keyword extracts the monadic value of a monadic expression.
newtype Monadic m r a = Monadic (m a)

instance Prelude.Monad m => PolyCont (Monadic m) (m b) a where
  runPolyCont (Monadic k) = (Prelude.>>=) k
