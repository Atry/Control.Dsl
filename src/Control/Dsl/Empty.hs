{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Dsl.Empty where

import Control.Dsl.PolyCont
import Data.Void
import qualified Control.Applicative
import Prelude hiding ((>>), (>>=), return, fail)

data Empty r a where
  Empty :: Empty r Void

instance {-# OVERLAPS #-} Control.Applicative.Alternative m => StatefulPolyCont Empty (m a) (m a) Void where
  runPolyCont Empty _ = Control.Applicative.empty

{- | Return an empty @a@, similar to 'Control.Applicative.empty'.

This 'empty' function aims to be used as the last statement of a @do@ block.
-}
empty :: forall a. PolyCont Empty a Void => a
empty = runPolyCont Empty (absurd @a)
