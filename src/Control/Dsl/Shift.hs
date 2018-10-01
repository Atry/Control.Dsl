{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Description : Delimited continuations
-}
module Control.Dsl.Shift where

import Data.Void
import Control.Dsl.Cont
import Control.Dsl.Dsl
import Prelude hiding ((>>), (>>=), return)

{- |
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
  i <- Shift $ earlyGenerator True
  Yield "after earlyGenerator"
  Yield $ "the return value of earlyGenerator is " ++ show i
  empty
:}

>>> earlyGeneratorTest
["before earlyGenerator","inside earlyGenerator","early return","after earlyGenerator","the return value of earlyGenerator is 1"]
-}
newtype Shift r0 r a = Shift (r0 !! a)

instance Dsl (Shift r) r a where
  cpsApply (Shift k) f = k f

-- instance Dsl m a d => Dsl m a (Shift d b) where
--   cpsApply k f = Shift $ \g -> cpsApply k $ \a -> cpsApply (f a) g
