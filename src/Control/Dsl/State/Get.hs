{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Control.Dsl.State.Get where

import           Prelude                 hiding ( (>>)
                                                , (>>=)
                                                , return
                                                , fail
                                                )
import           Control.Dsl.PolyCont
import           Control.Dsl.State.State

data Get r a where
  Get :: forall s r. Get r s

instance PolyCont Get (State s r) s where
  runPolyCont Get f s = f s s
