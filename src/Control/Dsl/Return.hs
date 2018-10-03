{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE GADTs #-}

module Control.Dsl.Return where

import Prelude hiding ((>>), (>>=), return)
import Control.Dsl.PolyCont
import Control.Exception
import Data.Void

data Return a r b where
  Return :: a -> Return a r Void

instance PolyCont (Return a) a Void where
  runPolyCont (Return a) _ = a

{- | Lift a value to the return type, similar to 'Prelude.return'.

When this 'return' is present in a nested @do@ block for 'when' or 'unless',
if the return value is not @()@,
it will create a 'Cont' that performs early return,
skipping the rest statements of the outer @do@ notation.

==== __Examples__

>>> :set -XTypeOperators
>>> :set -XRebindableSyntax
>>> import Prelude hiding ((>>), (>>=), return)
>>> import Control.Dsl
>>> import Control.Dsl.Return
>>> import Control.Dsl.Yield
>>> import Control.Dsl.Cont
>>> import Control.Dsl.Empty

>>> :{
earlyGenerator :: Bool -> Cont [String] Integer
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
  i <- earlyGenerator True
  Yield "after earlyGenerator"
  Yield $ "the return value of earlyGenerator is " ++ show i
  empty
:}

>>> earlyGeneratorTest
["before earlyGenerator","inside earlyGenerator","early return","after earlyGenerator","the return value of earlyGenerator is 1"]
-}
return a = runPolyCont (Return a) absurd

instance {-# OVERLAPS #-} Applicative m => PolyCont (Return a) (m a) Void where
  runPolyCont (Return a) _ = pure a
