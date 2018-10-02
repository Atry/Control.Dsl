{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Control.Dsl.Dsl where

import Control.Dsl.PolyCont
import Control.Dsl.Cont
import Prelude hiding ((>>), (>>=), return)

{- An use case of a keyword in a @do@ block.

A keyword is a delimited continuation,
which can be either ad-hoc polymorphic or not.

Don't create custom instances of 'Dsl' for keywords.
Instead, create 'PolyCont' for both your custom keywords and built-in keywords. 
-}
class Dsl k r a where
  (>>=) :: k r a -> r !! a
  (>>) :: k r a -> r -> r
  k >> a = k >>= const a

-- | Keywords based on ad-hoc polymorphic delimited continuation.
instance {-# OVERLAPPABLE #-} PolyCont k r a => Dsl k r a where
  (>>=) = runPolyCont

-- | Keywords based on monomorphic delimited continuation.
instance Dsl Cont r a where
  (>>=) = runCont

