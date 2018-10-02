{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Control.Dsl.Return where

import Prelude hiding ((>>), (>>=), return)
import Control.Dsl.Dsl
import Control.Dsl.Cont
import Control.Exception
import Data.Void

data Return r0 r b where
  Return :: r0 -> Return r0 r Void

instance Dsl (Return r) r Void where
  cpsApply (Return r) _ = r

return r = cpsApply (Return r) absurd

instance Dsl (Return a) (r !! a) Void where
  cpsApply (Return a) _ f = f a

instance Dsl (Return r) [r] Void where
  cpsApply (Return r) _ = [r]

instance Dsl (Return r) (Maybe r) Void where
  cpsApply (Return r) _ = Just r

instance Dsl (Return r) (IO r) Void where
  cpsApply (Return r) _ = evaluate r
