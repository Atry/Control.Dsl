{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Dsl.Internal.Maybe where

import Prelude (Maybe(..))
import Control.Dsl.Return
import Control.Dsl.Internal

instance Dsl (Return (Maybe a)) a d => Dsl Maybe a d where
  Nothing >>= f = (Return Nothing :: Return (Maybe a) a) >>= f
  Just a >>= f = f a
  