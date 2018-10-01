{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}

module Control.Dsl.Internal.Dsl where

import Control.Applicative
import Data.Void
import Prelude hiding ((>>), (>>=), return)

class Dsl m a d where
  cpsApply :: m a -> (a -> d) -> d
  (>>=) :: m a -> (a -> d) -> d
  (>>) :: m a -> d -> d
  (>>=) = cpsApply
  ma >> d = ma >>= \a -> d

instance {-# INCOHERENT #-} Dsl m a d => Dsl m a (b -> d) where
  cpsApply k f b = cpsApply k $ \a -> f a b

instance {-# INCOHERENT #-} (Applicative m, Dsl n Void d) => Dsl n Void (m d) where
  cpsApply k _ = pure $ cpsApply k absurd
