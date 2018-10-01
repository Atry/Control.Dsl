{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Dsl.Empty where

import Control.Dsl.Dsl
import Data.Void
import Control.Applicative
import Prelude hiding ((>>), (>>=), return)

data Empty r a where
  Empty :: Empty r Void

instance {-# OVERLAPPABLE #-} Monoid r => Dsl Empty r Void where
  cpsApply Empty _ = mempty

instance {-# OVERLAPPABLE #-} Alternative m => Dsl Empty (m b) Void where
  cpsApply Empty _ = Control.Applicative.empty
      
empty :: Dsl Empty r Void => r
empty = cpsApply Empty absurd