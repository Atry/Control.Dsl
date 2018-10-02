{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Control.Dsl.Empty where

import Control.Dsl.Cont
import Control.Dsl.Dsl
import Data.Void
import Control.Applicative hiding (empty)
import Prelude hiding ((>>), (>>=), return)

data Empty r a where
  Empty :: Empty r Void

instance Dsl Empty [r] Void where
  cpsApply Empty _ = []

instance Dsl Empty (Maybe r) Void where
  cpsApply Empty _ = Nothing

empty :: Dsl Empty r Void => r
empty = cpsApply Empty absurd

guard True = Cont $ \f -> f ()
guard False = Cont $ \f -> empty
