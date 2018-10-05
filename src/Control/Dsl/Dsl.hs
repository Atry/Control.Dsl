{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Control.Dsl.Dsl where

import Control.Dsl.PolyCont
import Control.Dsl.Cont
import Prelude hiding ((>>), (>>=), return, fail)

{- | An use case of a statement in a @do@ block.

== Allowed statements in DSL @do@ blocks

A statement in a DSL @do@ block is a delimited continuation,
which can be a GADT keyword, a control flow operator,
or the final result:

+-------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------+---------------------------------------------+
|                   |                                                                                              Keywords                                                                                              |          Control flow operators         |                   Results                   |
+-------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------+---------------------------------------------+
|      Examples     | 'Control.Dsl.Shift.Shift','Control.Dsl.Yield.Yield','Control.Dsl.State.Get.Get','Control.Dsl.State.Put.Put', 'Control.Dsl.Monadic.Monadic', 'Control.Dsl.Return.Return', 'Control.Dsl.Empty.Empty' | 'ifThenElse', 'when', 'unless', 'guard' | 'return', 'fail', 'Control.Dsl.Empty.empty' |
+-------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------+---------------------------------------------+
|    Defined as a   |                                                                                                GADT                                                                                                |                 function                |                   function                  |
+-------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------+---------------------------------------------+
|   Interpreted by  |                                                                                   'Control.Dsl.PolyCont.PolyCont'                                                                                  |         'Control.Dsl.Cont.Cont'         |       'Control.Dsl.PolyCont.PolyCont'       |
+-------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------+---------------------------------------------+
| Can be present as |                                                                                                    not the last statement of a @do@ block                                                                                                    |      the last statement of a @do@ block     |
+-------------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+---------------------------------------------+

Don't create custom instances of 'Dsl' for statement.
Instead, create 'PolyCont' instances for your custom GADT keywords.

==== __Examples__

>>> :set -XGADTs
>>> :set -XMultiParamTypeClasses
>>> :set -XFlexibleInstances
>>> :set -XFlexibleContexts
>>> :set -XRebindableSyntax
>>> :set -XTypeApplications
>>> import qualified Prelude
>>> import Prelude hiding ((>>), (>>=), return, fail)
>>> import Control.Dsl
>>> import Control.Dsl.State.Get
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

@f@ is a script that contains keywords of
'Control.Dsl.State.Get.Get',
'Control.Dsl.Yield.Yield',
and 'Control.Dsl.Return.return'.
With the help of built-in 'PolyCont' instances for those keywords,
@f@ can be used as a function that accepts a boolean parameter.

>>> f False :: [String]
["foo","baz"]

>>> f True :: [String]
["foo","bar","baz"]

In fact, @f@ can be any type
as long as 'PolyCont' instances for involved keywords are provided.

>>> :type f
f :: (PolyCont (Yield [Char]) r (),
      PolyCont (Return [Char]) r Void, PolyCont Get r Bool) =>
     r

For example, @f@ can be interpreted as an impure @IO ()@,
providing the following instances:

>>> :{
instance PolyCont (Yield String) (IO ()) () where
  runPolyCont (Yield a) = (Prelude.>>=) (putStrLn $ "Yield " ++ a)
instance PolyCont Get (IO ()) Bool where
  runPolyCont Get f = putStrLn "Get" Prelude.>> f False
instance PolyCont (Return String) (IO ()) Void where
  runPolyCont (Return r) _ = putStrLn $ "Return " ++ r
:}

>>> f :: IO ()
Yield foo
Get
Return baz
-}
class Dsl k r a where
  cpsApply :: k r a -> (a -> r) -> r

{- | The implementation of @<-@ statements in a @do@ block,
which forwards to 'runCont' if @k@ is 'Cont',
otherwise forwards to 'runPolyCont'.
-}
(>>=) k = cpsApply k

f =<< k = k >>= f

(f >=> g) k = f k >>= g

f <=< g = f >=> g

-- | The implementation of statements with no value in a @do@ block.
k >> a = cpsApply k $ const a

-- | Statements based on ad-hoc polymorphic delimited continuations.
instance {-# OVERLAPS #-} PolyCont k r a => Dsl k r a where
  cpsApply = runPolyCont

-- | Statements based on monomorphic delimited continuations.
instance Dsl Cont r a where
  cpsApply = runCont

forever :: Dsl k r a => k r a -> r
forever k = k >> forever k

ifThenElse True k _ = k
ifThenElse False _ k = k
