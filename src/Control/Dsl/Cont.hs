{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Description : Delimited continuations
-}
module Control.Dsl.Cont where

import Data.Void
import Control.Dsl.Internal.Dsl
import Prelude hiding ((>>), (>>=), return)

type Cont r a = (a -> r) -> r
type r !! a = Cont r a

{- |
>>> :set -XTypeOperators
>>> :set -XRebindableSyntax
>>> import Prelude hiding ((>>), (>>=), return)
>>> import Control.Dsl
>>> import Control.Dsl.Return
>>> import Control.Dsl.Yield
>>> import Control.Dsl.Empty

>>> :{
earlyGenerator :: [String] !! Integer
earlyGenerator = do
  Yield "before Return"
  Return 1
  undefined
:}

>>> :{
earlyGeneratorTest :: [String]
earlyGeneratorTest = do
  Yield "before earlyGenerator"
  i <- Shift earlyGenerator
  Yield "after earlyGenerator"
  Yield $ "the return value of earlyGenerator is " ++ show i
  Empty
  undefined
:}

>>> earlyGeneratorTest
["before earlyGenerator","before Return","after earlyGenerator","the return value of earlyGenerator is 1"]
-}
newtype Shift r a = Shift (Cont r a)

instance Dsl (Shift r) a r where
  Shift k >>= f = k f

instance {-# INCOHERENT #-} (Dsl m Void d) => Dsl m Void (c !! d) where
  (k >>= _) f = f $ k >>= absurd

