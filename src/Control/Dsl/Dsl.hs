{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Control.Dsl.Dsl where

import Control.Dsl.Cont
import Control.Applicative
import Data.Void
import Prelude hiding ((>>), (>>=), return)

{- | This type class witnesses a use case of the keyword @k@,
which is an ad-hoc delimited continuation adaptive to the answer type @r@.
-}
class Dsl k r a where
  -- | Run a keyword as a CPS-function.
  cpsApply :: k r0 a -> r !! a

instance {-# OVERLAPPABLE #-} Dsl k r a => Dsl k (b -> r) a where
  cpsApply k f b = cpsApply k $ \a -> f a b
