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
import Prelude hiding ((>>), (>>=), return, fail)

{- | A type alias to 'Cont' for a deeply nested delimited continuation.

==== __Examples__

>>> :set -XTypeOperators
>>> :set -XRebindableSyntax
>>> import Prelude hiding ((>>), (>>=), return, fail)
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
type (!!) = Cont

-- | A delimited continuation that can be used in a @do@ block.
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

-- | Convert a 'PolyCont' to a 'Cont'.
toCont k = Cont (runPolyCont k)

when :: Bool -> Cont r () -> Cont r ()
when True k = k
when False _ = Cont ($ ())

unless True _ = Cont ($ ())
unless False k = k

guard True = Cont ($ ())
guard False = Cont (const empty)

instance {-# OVERLAPS #-} PolyCont k r a => PolyCont k (Cont r a') a where
  runPolyCont k f = Cont $ \g -> runPolyCont k $ \a -> runCont (f a) g

instance PolyCont (Return r) (Cont r' r) Void where
  runPolyCont (Return r) _ = Cont ($ r)

instance PolyCont Empty r Void => PolyCont Empty (Cont r a) Void where
  runPolyCont k _ = Cont (const empty)
