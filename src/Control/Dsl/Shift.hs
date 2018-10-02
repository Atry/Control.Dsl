{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Description : Delimited continuations
-}
module Control.Dsl.Shift where

import Data.Void
import Control.Dsl.Cont
import Control.Dsl.Dsl
import Prelude hiding ((>>), (>>=), return)

newtype Shift r0 r a = Shift (r0 !! a)

instance Dsl (Shift r) r a where
  cpsApply (Shift k) f = k f
