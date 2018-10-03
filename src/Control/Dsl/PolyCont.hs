{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Control.Dsl.PolyCont where

import Prelude hiding ((>>), (>>=), return)
import Control.Dsl.Cont

{- | A use case of an __ad-hoc polymorphic delimited continuation__.

Note that a 'PolyCont' is not a __polymorphic delimited continuation__,
since a 'PolyCont' does not support answer type modification.
-}
class PolyCont k r a where
  -- | Run as a CPS function .
  runPolyCont :: k d a -> (a -> r) -> r

instance {-# OVERLAPS #-} PolyCont k r a => PolyCont k (b -> r) a where
  runPolyCont k f b = runPolyCont k $ \a -> f a b
      
instance {-# OVERLAPS #-} PolyCont k r a => PolyCont k (Cont r b) a where
  runPolyCont k f = Cont $ \g -> runPolyCont k $ \a -> runCont (f a) g