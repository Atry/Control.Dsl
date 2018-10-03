{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE GADTs #-}

module Control.Dsl.State.Put where

import Prelude hiding ((>>), (>>=), return)
import Control.Dsl.PolyCont
import Control.Dsl.State.State

data Put a r u where
  Put :: a -> Put a r ()

instance PolyCont (Put a) (State a b) () where
  runPolyCont (Put a) f _ = f () a
