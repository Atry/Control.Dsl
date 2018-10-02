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
    ($ ())
  return "baz"
:}

>>> :type f
f :: (PolyCont (Yield [Char]) r (),
      PolyCont (Return [Char]) r Void, PolyCont Get r Bool) =>
     r

>>> f True :: [String]
["foo","bar","baz"]

>>> f False :: [String]
["foo","baz"]

>>> :{
instance PolyCont (Yield String) (IO ()) () where
  runPolyCont (Yield a) = (Prelude.>>=) (putStrLn $ "Yield " ++ a)
:}

>>> :{
instance PolyCont Get (IO ()) Bool where
  runPolyCont Get f = (putStrLn "Get") Prelude.>> f False
:}

>>> :{
instance PolyCont (Return String) (IO ()) Void where
  runPolyCont (Return a) _ = putStrLn $ "Return " ++ a
:}

>>> f :: IO ()
Yield foo
Get
Return baz
-}
module Control.Dsl(
  module Control.Dsl.Return,
  module Control.Dsl.Dsl,
  module Control.Dsl.Empty,
  module Control.Dsl.Cont
) where

import Control.Dsl.Return (return)
import Control.Dsl.Cont (when, unless)
import Control.Dsl.Dsl
import Control.Dsl.Empty (guard)
import Prelude hiding ((>>), (>>=), return)
