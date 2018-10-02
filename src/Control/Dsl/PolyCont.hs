{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Control.Dsl.PolyCont where

import Control.Dsl.Cont
import Control.Applicative
import Data.Void
import Prelude hiding ((>>), (>>=), return)

{- | A use case of an __ad-hoc polymorphic delimited continuation__.

Note that 'PolyCont's are not __polymorphic delimited continuation__,
as 'PolyCont's do not support answer type modification.
-}
class PolyCont k r a where
  -- | Run as a CPS function .
  runPolyCont :: k r0 a -> r !! a

instance {-# OVERLAPPABLE #-} PolyCont k r a => PolyCont k (b -> r) a where
  runPolyCont k f b = runPolyCont k $ \a -> f a b
