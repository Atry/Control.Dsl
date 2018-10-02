{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Control.Dsl.Return where

import Prelude hiding ((>>), (>>=), return)
import Control.Dsl.PolyCont
import Control.Dsl.Cont
import Control.Exception
import Data.Void

data Return r0 r b where
  Return :: r0 -> Return r0 r Void

instance PolyCont (Return r) r Void where
  runPolyCont (Return r) _ = r

{- | Lift a value to the return type, similar to 'Control.Monad.return'.

When this 'return' is present in a nested @do@ block for 'when' or 'unless',
if the return value is not @()@,
it will create a '!!' that performs early return,
skipping the rest statements of the outer @do@ notation.

==== __Examples__

>>> :set -XTypeOperators
>>> :set -XRebindableSyntax
>>> import Prelude hiding ((>>), (>>=), return)
>>> import Control.Dsl
>>> import Control.Dsl.Return
>>> import Control.Dsl.Yield
>>> import Control.Dsl.Empty

>>> :{
earlyGenerator :: Bool -> [String] !! Integer
earlyGenerator earlyReturn = do
  Yield "inside earlyGenerator"
  when earlyReturn $ do
    Yield "early return"
    return 1
  Yield "normal return"
  return 0
:}

>>> :{
earlyGeneratorTest :: [String]
earlyGeneratorTest = do
  Yield "before earlyGenerator"
  i <- Cont $ earlyGenerator True
  Yield "after earlyGenerator"
  Yield $ "the return value of earlyGenerator is " ++ show i
  empty
:}

>>> earlyGeneratorTest
["before earlyGenerator","inside earlyGenerator","early return","after earlyGenerator","the return value of earlyGenerator is 1"]
-}
return r = runPolyCont (Return r) absurd

instance PolyCont (Return a) (r !! a) Void where
  runPolyCont (Return a) _ f = f a

instance PolyCont (Return r) [r] Void where
  runPolyCont (Return r) _ = [r]

instance PolyCont (Return r) (Maybe r) Void where
  runPolyCont (Return r) _ = Just r

instance PolyCont (Return r) (IO r) Void where
  runPolyCont (Return r) _ = evaluate r
