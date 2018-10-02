{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Control.Dsl.Do where

import Control.Dsl.Dsl
import Control.Dsl.Cont
import Prelude hiding ((>>), (>>=), return)

class Do k r a where
  (>>=) :: k r a -> r !! a
  (>>) :: k r a -> r -> r
  k >> a = k >>= const a

instance {-# OVERLAPPABLE #-} Dsl k r a => Do k r a where
  (>>=) = cpsApply

instance Do Cont r a where
  (>>=) = runCont

