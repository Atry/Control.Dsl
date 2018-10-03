{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE GADTs #-}

module Control.Dsl.State.Put where

import Prelude hiding ((>>), (>>=), return)
import Control.Dsl.PolyCont
import Control.Dsl.State.State

data Put s r a where
  Put :: s -> Put s r ()

instance PolyCont (Put s) (State s r) () where
  runPolyCont (Put s) f _ = f () s
