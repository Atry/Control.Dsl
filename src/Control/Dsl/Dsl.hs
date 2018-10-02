{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Control.Dsl.Dsl where

import Control.Dsl.PolyCont
import Control.Dsl.Cont
import Prelude hiding ((>>), (>>=), return)

{- | An use case of a keyword in a @do@ block.

A keyword is a delimited continuation,
which can be either ad-hoc polymorphic or not.

Don't create custom instances of 'Dsl' for keywords.
Instead, create 'PolyCont' for both your custom keywords and built-in keywords. 

==== __Examples__

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
  runPolyCont Get f = putStrLn "Get" Prelude.>> f False
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
class Dsl k r a where
  (>>=) :: k r a -> r !! a
  (>>) :: k r a -> r -> r
  k >> a = k >>= const a

-- | Keywords based on ad-hoc polymorphic delimited continuations.
instance {-# OVERLAPPABLE #-} PolyCont k r a => Dsl k r a where
  (>>=) = runPolyCont

-- | Keywords based on monomorphic delimited continuations.
instance Dsl Cont r a where
  (>>=) = runCont

