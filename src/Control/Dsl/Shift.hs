
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Description : Delimited continuations
-}
module Control.Dsl.Shift where

import Data.Void
import Control.Dsl.Cont
import Control.Dsl.PolyCont
import Prelude hiding ((>>), (>>=), return)

-- | A keyword to extract the value of a CPS function .
newtype Shift d r a = Shift ((a -> d) -> d)

instance PolyCont (Shift r) r a where
  runPolyCont (Shift k) = k
