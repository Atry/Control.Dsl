{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
>>> :set -XTypeFamilies
>>> :set -XMultiParamTypeClasses
>>> :set -XFlexibleInstances
>>> :set -XFlexibleContexts
>>> :set -XRebindableSyntax
>>> :set -XTypeApplications
>>> import qualified Prelude
>>> import Prelude hiding ((>>), (>>=), return)
>>> import Control.Dsl
>>> import Control.Dsl.State
>>> import Control.Dsl.Yield
>>> import Control.Dsl.Return
>>> import Data.Void

>>> :{
f = do
  Yield "foo"
  config <- Get @Bool
  when config $ do
    Yield "bar"
    return ()
  return "baz"
:}

>>> :type f
f :: (Dsl (Yield [Char]) r (), Dsl (Return [Char]) r Void,
      Dsl Get r Bool) =>
     r

>>> f True :: [String]
["foo","bar","baz"]

>>> f False :: [String]
["foo","baz"]

>>> :{
instance Dsl (Yield String) (IO ()) () where
  cpsApply (Yield a) = (Prelude.>>=) (putStrLn $ "Yield " ++ a)
:}

>>> :{
instance Dsl Get (IO ()) Bool where
  cpsApply Get f = (putStrLn "Get") Prelude.>> f False
:}

>>> :{
instance Dsl (Return String) (IO ()) Void where
  cpsApply (Return a) _ = putStrLn $ "Return " ++ a
:}

>>> f :: IO ()
Yield foo
Get
Return baz
-}
module Control.Dsl(
  module Control.Dsl.Dsl,
  module Control.Dsl.Return,
  module Control.Dsl.Do,
  module Control.Dsl.Cont
) where

import Control.Dsl.Return (return)
import Control.Dsl.Dsl (Dsl(..))
import Control.Dsl.Cont (when)
import Control.Dsl.Do ((>>=), (>>))

-- Import modules that contains orphan instances
import Control.Dsl.Shift ()
-- import Control.Dsl.State ()