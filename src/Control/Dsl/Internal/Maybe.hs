{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Control.Dsl.Internal.Maybe where

import Prelude hiding ((>>), (>>=), return)
import Control.Dsl.Return
import Control.Dsl.Internal

instance Dsl (Return (Maybe a)) d d => Dsl Maybe a d where
  Nothing >>= f = return @(Maybe a) Nothing
  Just a >>= f = f a
