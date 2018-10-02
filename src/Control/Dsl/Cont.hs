{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Control.Dsl.Cont where

import Prelude hiding ((>>), (>>=), return)

type r !! a = (a -> r) -> r

newtype Cont r a = Cont (r !! a)

when True k = Cont k
when False _ = Cont $ \f -> f ()