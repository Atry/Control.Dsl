{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Control.Dsl.Cont where

import Control.Dsl.Return
import Control.Dsl.Empty
import Control.Dsl.PolyCont
import Data.Void
import Prelude hiding ((>>), (>>=), return)

{- | A type alias to 'Cont' for a deeply nested delimited continuation.

==== __Examples__

>>> :set -XTypeOperators
>>> :set -XRebindableSyntax
>>> import Prelude hiding ((>>), (>>=), return)
>>> import Control.Dsl
>>> import Control.Dsl.Yield
>>> import Control.Dsl.Empty
>>> :{
f :: IO () !! [Integer] !! [String] !! [Double]
f = do
  Yield "foo"
  Yield 0.5
  Yield 42
  empty
:}
-}
type r !! a = Cont r a

-- ! A delimited continuation that can be used in a @do@ block.
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

when True (Cont k) = Cont k
when False _ = Cont $ \f -> f ()

unless True _ = Cont $ \f -> f ()
unless False (Cont k) = Cont k

guard True = Cont $ \f -> f ()
guard False = Cont $ \f -> empty

instance {-# OVERLAPS #-} PolyCont k r a => PolyCont k (Cont r b) a where
  runPolyCont k f = Cont $ \g -> runPolyCont k $ \a -> runCont (f a) g

instance PolyCont (Return a) (Cont r a) Void where
  runPolyCont (Return a) _ = Cont ($ a)

instance {-# OVERLAPS #-} PolyCont Empty r Void => PolyCont Empty (Cont r a) Void where
  runPolyCont k _ = Cont (const empty)
