{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.Dsl.PolyCont where

import Prelude hiding ((>>), (>>=), return, fail)

{- | A use case of an __ad-hoc polymorphic delimited continuation__.

Note that a 'PolyCont' is not a __polymorphic delimited continuation__,
since a 'PolyCont' does not support answer type modification.
-}
type PolyCont k r a = StatefulPolyCont k r r a

class StatefulPolyCont k i o a | k i a -> o where
  -- | Run as a CPS function .
  runPolyCont :: k i' a -> (a -> i) -> o

