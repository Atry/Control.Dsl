{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Control.Dsl.State.Get where

import Prelude hiding ((>>), (>>=), return)
import Control.Dsl.PolyCont
import Control.Dsl.State.State

data Get r a where
  Get :: forall a r. Get r a

instance PolyCont Get (State a b) a where
  runPolyCont Get f a = f a a