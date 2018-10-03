{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Control.Dsl.PolyCont where

import Prelude hiding ((>>), (>>=), return)

{- | A use case of an __ad-hoc polymorphic delimited continuation__.

Note that a 'PolyCont' is not a __polymorphic delimited continuation__,
since a 'PolyCont' does not support answer type modification.
-}
class PolyCont k r a where
  -- | Run as a CPS function .
  runPolyCont :: k d a -> (a -> r) -> r
