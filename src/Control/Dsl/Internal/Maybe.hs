{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Dsl.Internal.Maybe where

import Control.Dsl.Return
import Control.Dsl.Internal

instance Dsl (Return (Maybe a)) a d => Dsl Maybe a d where
  (>>=) Nothing handler = (Return Nothing :: Return (Maybe a) a) Control.Dsl.Internal.>>= handler
  (>>=) (Just a) handler = handler a
  