{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Control.Dsl.Cont where

import Prelude hiding ((>>), (>>=), return)

type r !! a = Cont r a

-- ! A delimited continuation that can be used in a @do@ block.
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

when True (Cont k) = Cont k
when False _ = Cont $ \f -> f ()

unless True _ = Cont $ \f -> f ()
unless False (Cont k) = Cont k
