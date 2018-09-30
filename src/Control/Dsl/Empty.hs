{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Dsl.Empty where

import Control.Dsl.Internal.Dsl
import Control.Applicative
import Prelude (Monoid, mempty)

data Empty a = Empty

instance {-# OVERLAPPABLE #-} Monoid d => Dsl Empty a d where
  (>>=) Empty _ = mempty

instance {-# INCOHERENT #-} Alternative m => Dsl Empty a (m b) where
  (>>=) Empty _ = empty
