{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Control.Dsl.Empty where

import Control.Dsl.Cont
import Control.Dsl.PolyCont
import Data.Void
import qualified Control.Applicative
import Prelude hiding ((>>), (>>=), return)

data Empty r a where
  Empty :: Empty r Void

instance {-# OVERLAPS #-} Control.Applicative.Alternative m => PolyCont Empty (m a) Void where
  runPolyCont Empty _ = Control.Applicative.empty

instance PolyCont Empty a Void => PolyCont Empty (b -> a) Void where
  runPolyCont Empty _ _ = empty

instance PolyCont Empty a Void => PolyCont Empty (Cont r a) Void where
  runPolyCont Empty _ = Cont ($ empty)

empty :: PolyCont Empty a Void => a
empty = runPolyCont Empty absurd

guard True = Cont $ \f -> f ()
guard False = Cont $ \f -> empty
