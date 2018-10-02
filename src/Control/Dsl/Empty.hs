{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Control.Dsl.Empty where

import Control.Dsl.Cont
import Control.Dsl.PolyCont
import Data.Void
import Control.Applicative hiding (empty)
import Prelude hiding ((>>), (>>=), return)

data Empty r a where
  Empty :: Empty r Void

instance PolyCont Empty [r] Void where
  runPolyCont Empty _ = []

instance PolyCont Empty (Maybe r) Void where
  runPolyCont Empty _ = Nothing

empty :: PolyCont Empty r Void => r
empty = runPolyCont Empty absurd

guard True = Cont $ \f -> f ()
guard False = Cont $ \f -> empty
