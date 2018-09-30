{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Dsl.Empty where

import Control.Dsl.Internal.Dsl
import Data.Void
import Control.Applicative
import Prelude hiding ((>>), (>>=), return)

data Empty a where
  Empty :: Empty Void

instance {-# OVERLAPPABLE #-} Monoid d => Dsl Empty Void d where
  (>>=) Empty _ = mempty

instance {-# INCOHERENT #-} Alternative m => Dsl Empty Void (m b) where
  (>>=) Empty _ = Control.Applicative.empty

empty :: Dsl Empty Void a => a
empty = Empty >>= absurd