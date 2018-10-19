{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Dsl.Dsl where

import Control.Dsl.PolyCont
import Control.Dsl.Cont
import Prelude hiding ((>>), (>>=), return, fail)

{- | Witnesses a use case of a statement in a @do@ block.

== Allowed statements in DSL @do@ blocks

Statements in a DSL @do@ block are delimited continuations
(except the last statement),
which can be either ad-hoc polymorphic GADT keywords,
or monomorphic control flow operators.

The last statement is the final result of the @do@ block,
or the /answer type/ of other delimited continuation statements.

+-------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------+----------------------------------------------------------------------------------------------+
|                   |                                                                                                Keywords                                                                                               |          Control flow operators         |                                            Results                                           |
+-------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------+----------------------------------------------------------------------------------------------+
|      Examples     | 'Control.Dsl.Shift.Shift', 'Control.Dsl.Yield.Yield', 'Control.Dsl.State.Get.Get', 'Control.Dsl.State.Put.Put', 'Control.Dsl.Monadic.Monadic', 'Control.Dsl.Return.Return', 'Control.Dsl.Empty.Empty' | 'ifThenElse', 'when', 'unless', 'guard' | 'Control.Dsl.Return.return', 'Control.Dsl.Return.fail', 'Control.Dsl.Empty.empty', 'forever' |
+-------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------+----------------------------------------------------------------------------------------------+
|        Type       |                                                                                              custom GADT                                                                                              |         'Control.Dsl.Cont.Cont'         |                                      the answer type @r@                                     |
+-------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------+----------------------------------------------------------------------------------------------+
|   Interpreted by  |                                                                                    'Control.Dsl.PolyCont.PolyCont'                                                                                    |                   N/A                   |                                'Control.Dsl.PolyCont.PolyCont'                               |
+-------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------+----------------------------------------------------------------------------------------------+
| Can be present at |                                                                                 not the last statement in a @do@ block                                                                                |       any position in a @do@ block      |                              the last statement in a @do@ block                              |
+-------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------+----------------------------------------------------------------------------------------------+

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

@f@ is a @do@ block that contains keywords of
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
f :: (StatefulPolyCont (Yield [Char]) o o (),
      StatefulPolyCont (Return [Char]) o o Void,
      StatefulPolyCont Get o o Bool) =>
     o

For example, @f@ can be interpreted as an impure @IO ()@,
providing the following instances:

>>> :{
instance StatefulPolyCont (Yield String) (IO ()) (IO ()) () where
  runPolyCont (Yield a) = (Prelude.>>=) (putStrLn $ "Yield " ++ a)
instance StatefulPolyCont Get (IO ()) (IO ()) Bool where
  runPolyCont Get f = putStrLn "Get" Prelude.>> f False
instance StatefulPolyCont (Return String) (IO ()) (IO ()) Void where
  runPolyCont (Return r) _ = putStrLn $ "Return " ++ r
:}

>>> f :: IO ()
Yield foo
Get
Return baz
-}
type Dsl k r a = StatefulDsl k r r a

class StatefulDsl k i o a | k i a -> o where
  cpsApply :: k i a -> (a -> i) -> o

{- | The implementation of @<-@ statements in a @do@ block,
which forwards to 'runCont' if @k@ is 'Cont',
otherwise forwards to 'runPolyCont' from 'PolyCont'.
-}
(>>=) k = cpsApply k

f =<< k = k >>= f

(f >=> g) k = f k >>= g

f <=< g = g >=> f

-- | The implementation of statements with no value in a @do@ block.
k >> a = cpsApply k $ const a

forever k = k >> forever k

ifThenElse True k _ = k
ifThenElse False _ k = k


-- | Statements based on ad-hoc polymorphic delimited continuations.
instance {-# OVERLAPS #-} StatefulPolyCont k r r a => StatefulDsl k r r a where
  cpsApply = runPolyCont

-- | Statements based on monomorphic delimited continuations.
instance StatefulDsl Cont r r a where
  cpsApply = runCont
