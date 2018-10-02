{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Control.Dsl.Cont where

import Prelude hiding ((>>), (>>=), return)

-- ! A CPS function 
type r !! a = (a -> r) -> r

-- ! A delimited continuation that can be used in a @do@ block.
newtype Cont r a = Cont { runCont :: r !! a }

when True k = Cont k
when False _ = Cont $ \f -> f ()

unless True _ = Cont $ \f -> f ()
unless False k = Cont k
