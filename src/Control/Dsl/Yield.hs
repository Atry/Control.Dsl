{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}

module Control.Dsl.Yield where

import Control.Dsl.Internal

data Yield a b where
  Yield :: a -> Yield a ()

instance Dsl (Yield a) () [a] where
  Yield a >>= f = a : f ()
