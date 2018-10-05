{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

{- | This "Control.Dsl" module and its submodules provide a toolkit to create extensible Domain Specific Languages in @do@-notation.

A DSL @do@ block can contain heterogeneous statements from different vendors. Each statement is a keyword interpreted by a 'Dsl' type class instance, either effectful or purely. A DSL @do@ block is abstract. The type class requirements can be automatically inferred. Therefore, the data structures and implementation of interpreters can be switched by providing different instances.

= Getting started

This package provides 'Dsl' type class used in @do@ notation,
as a replacement to 'Control.Monad.Monad'.

@RebindableSyntax@ extension is required to enable DSL @do@ notation.

>>> :set -XRebindableSyntax
>>> import Prelude hiding ((>>), (>>=), return, fail)
>>> import Control.Dsl

== DSL model

Suppose you are creating a DSL for console IO,
you need to define some keywords allowed in the DSL.

Each keyword is a GADT:

>>> data MaxLengthConfig r a where MaxLengthConfig :: MaxLengthConfig r Int
>>> data GetLine r a where GetLine :: GetLine r String
>>> data PutStrLn r a where PutStrLn :: String -> PutStrLn r ()

== DSL script

Then those keywords can be used in @do@ blocks:

>>> :{
dslScript = do
  maxLength <- MaxLengthConfig
  line1 <- GetLine
  line2 <- GetLine
  when (length line1 + length line2 > maxLength) $ do
    PutStrLn "The input is too long"
    fail "Illegal input"
  PutStrLn ("The input is " ++ line1 ++ " and " ++ line2)
  return ()
:}

The above @dslScript@ function creates a DSL \"script\"
from keywords and some built-in control flow functions.

Keywords and the result statement 'return' and 'failed'
are ad-hoc polymorphic delimited continuations,
interpreted by 'Dsl.Contro.PolyCont.PolyCont'.

The type of @r@ varies for different instances of 'Dsl.Contro.PolyCont.PolyCont',
as show below:

>>> :type dslScript
dslScript
  :: (PolyCont (Return IOError) r Void, PolyCont (Return ()) r Void,
      PolyCont MaxLengthConfig r Int, PolyCont GetLine r [Char],
      PolyCont PutStrLn r ()) =>
     r

==== Pure interpreter

>>> type PureInterpreter = Int -> [String] -> Cont [String] IOError

>>> :{
instance PolyCont MaxLengthConfig PureInterpreter Int where
  runPolyCont MaxLengthConfig = runPolyCont Get
:}

>>> :{
instance PolyCont PutStrLn PureInterpreter () where
  runPolyCont (PutStrLn s) = runPolyCont (Yield s)
:}

>>> :{
instance PolyCont (Return ()) PureInterpreter Void where
  runPolyCont (Return ()) = runPolyCont Empty
:}

>>> :{
instance PolyCont GetLine PureInterpreter String where
  runPolyCont k = runCont $ do
    x : xs <- Get @[String]
    Put xs
    return x
:}

>>> runScriptPurely = dslScript :: PureInterpreter

>>> errorHandler e = ["(handled) " ++ show e]
>>> runCont (runScriptPurely 80 ["LINE_1", "LINE_2"]) errorHandler
["The input is LINE_1 and LINE_2"]

>>> longInput = [replicate 40 '*', replicate 41 '*']
>>> runCont (runScriptPurely 80 longInput) errorHandler
["The input is too long","(handled) user error (Illegal input)"]

>>> runCont (runScriptPurely 80 ["ONE_LINE"]) errorHandler
["(handled) user error (Pattern match failure in do expression at <interactive>..."]

==== Effectful interpreter

>>> type EffectfulInterpreter = Handle -> IO ()

>>> :{
instance PolyCont GetLine EffectfulInterpreter String where
  runPolyCont GetLine = runCont $ do
    h <- Get
    line <- Monadic (hGetLine h)
    return line
:}

>>> :{
instance PolyCont MaxLengthConfig (IO ()) Int where
  runPolyCont MaxLengthConfig f = f 80
:}

>>> :{
instance PolyCont PutStrLn (IO ()) () where
  runPolyCont (PutStrLn s) = (Prelude.>>=) (putStrLn s)
:}

>>> :{
instance PolyCont (Return IOError) (IO ()) Void where
  runPolyCont (Return e) _ = hPutStrLn stderr (show e)
:}

>>> runScriptEffectfully = dslScript :: EffectfulInterpreter

>>> :{
withSystemTempFile "tmp-input-file" $ \_ -> \h -> do
  Monadic $ hPutStrLn h "LINE_1"
  Monadic $ hPutStrLn h "LINE_2"
  Monadic $ hSeek h AbsoluteSeek 0
  runScriptEffectfully h
:}
The input is LINE_1 and LINE_2

== Allowed statements in DSL @do@ blocks

A DSL @do@ block contains keywords, control flow operators,
and the final result:

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
-}
module Control.Dsl(
  module Control.Dsl.Dsl,
  module Control.Dsl.Return,
  module Control.Dsl.Cont
) where

import Control.Dsl.Dsl hiding (cpsApply)
import Control.Dsl.Return (return, fail)
import Control.Dsl.Cont (when, unless, guard)

import Control.Dsl.State -- For orphan instances

import qualified Control.Monad -- For resolving haddock links
import qualified Control.Dsl.PolyCont -- For resolving haddock links
import qualified Control.Dsl.Cont -- For resolving haddock links

-- $setup
-- >>> :set -XGADTs
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -XUndecidableInstances
-- >>> :set -XTypeApplications
-- >>> import qualified Prelude
-- >>> import Control.Dsl.PolyCont
-- >>> import Control.Dsl.Monadic
-- >>> import Control.Dsl.Cont
-- >>> import Control.Dsl.Yield
-- >>> import Control.Dsl.Return
-- >>> import Control.Dsl.Empty
-- >>> import Data.Void
-- >>> import System.IO
-- >>> import System.IO.Temp
