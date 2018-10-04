{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE GADTs #-}

module Control.Dsl.Return where

import Prelude hiding ((>>), (>>=), return, fail)
import Control.Dsl.PolyCont
import Control.Exception
import Data.Void

data Return r' r a where
  Return :: r' -> Return r' r Void

instance PolyCont (Return r) r Void where
  runPolyCont (Return r) _ = r

{- | Lift @r@ to the answer type, similar to 'Prelude.return'.

This 'return' function aims to be used as the last statement of a @do@ block.

When 'return' is present in a nested @do@ block for 'when' or 'unless',
if the @r@ is not @()@,
it will create a 'Cont' that performs early return,
skipping the rest statements of the outer @do@ notation.

==== __Examples__

>>> :set -XTypeOperators
>>> :set -XRebindableSyntax
>>> import Prelude hiding ((>>), (>>=), return, fail)
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
return r = runPolyCont (Return r) absurd

{- | Lift an 'IOError' to the answer type, similar to 'Prelude.fail'.

This 'fail' function aims to be used as the last statement of a @do@ block.
-}
fail r = return (userError r)

instance {-# OVERLAPS #-} Applicative m => PolyCont (Return r) (m r) Void where
  runPolyCont (Return r) _ = pure r
