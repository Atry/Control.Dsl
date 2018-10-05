{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

{- | This "Control.Dsl" module and its submodules provide a toolkit
to create extensible Domain Specific Languages in @do@-notation.

A DSL @do@ block contains heterogeneous statements from different vendors.
A statement can be defined as a GADT,
interpreted by a 'Dsl' type class instance, either effectfully or purely.

A DSL @do@ block is abstract.
When creating the block, the type class requirements is automatically inferred.
Therefore, the data structures and implementation of interpreters
can be switched by providing different instances.

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

== DSL @do@ block

Then those keywords can be used in @do@ blocks:

>>> :{
dslBlock = do
  maxLength <- MaxLengthConfig
  line1 <- GetLine
  line2 <- GetLine
  when (length line1 + length line2 > maxLength) $ do
    PutStrLn "The input is too long"
    fail "Illegal input"
  PutStrLn ("The input is " ++ line1 ++ " and " ++ line2)
  return ()
:}

The above @dslBlock@ function creates an abstract code block of DSL 
from keywords and some built-in control flow functions.

Keywords and the result statement 'return' and 'fail'
are ad-hoc polymorphic delimited continuations,
interpreted by 'Control.Dsl.PolyCont.PolyCont',
which can be automatically inferred:

>>> :type dslBlock
dslBlock
  :: (PolyCont (Return IOError) r Void, PolyCont (Return ()) r Void,
      PolyCont MaxLengthConfig r Int, PolyCont GetLine r [Char],
      PolyCont PutStrLn r ()) =>
     r

=== Creating a pure interpreter

The type @r@ varies from different 'Control.Dsl.PolyCont.PolyCont' instances.
By defining 'Control.Dsl.PolyCont.PolyCont' instances for @PureInterpreter@,
you can make @r@ be a @PureInterpreter@:

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

The above three 'Control.Dsl.PolyCont.PolyCont' instances are implemented as
forwarders to other existing keywords.

>>> :{
instance PolyCont GetLine PureInterpreter String where
  runPolyCont k = runCont $ do
    x : xs <- Get @[String]
    Put xs
    return x
:}
...

The 'Control.Dsl.PolyCont.PolyCont' instance for @GetLine@ is implemented as a
'Control.Dsl.Cont.Cont' that contains a DSL @do@ block of atomic statements.

=== Running the DSL purely

>>> runPurely = dslBlock :: PureInterpreter

>>> errorHandler e = ["(handled) " ++ show e]
>>> runCont (runPurely 80 ["LINE_1", "LINE_2"]) errorHandler
["The input is LINE_1 and LINE_2"]

>>> longInput = [replicate 40 '*', replicate 41 '*']
>>> runCont (runPurely 80 longInput) errorHandler
["The input is too long","(handled) user error (Illegal input)"]

>>> runCont (runPurely 80 ["ONE_LINE"]) errorHandler
["(handled) user error (Pattern match failure in do expression at <interactive>..."]

=== Creating an effectful interpreter

Alternatively, @dslBlock@ can run effectfully by providing effectful
'Control.Dsl.PolyCont.PolyCont' instances.

>>> type EffectfulInterpreter = Handle -> IO ()

>>> :{
instance PolyCont GetLine EffectfulInterpreter String where
  runPolyCont GetLine = runCont $ do
    h <- Get
    line <- Monadic (hGetLine h)
    return line
:}

'Control.Dsl.Monadic.Monadic' is a built-in keyword to perform old-fashioned
monadic action in a DSL @do@ block.

Other keywords can be used together with 'Control.Dsl.Monadic.Monadic'.
No monad transformer is required.

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

=== Running the DSL effectfully

>>> runEffectfully = dslBlock :: EffectfulInterpreter

>>> :{
withSystemTempFile "tmp-input-file" $ \_ -> \h -> do
  Monadic $ hPutStrLn h "LINE_1"
  Monadic $ hPutStrLn h "LINE_2"
  Monadic $ hSeek h AbsoluteSeek 0
  runEffectfully h
:}
The input is LINE_1 and LINE_2
-}
module Control.Dsl(
  module Control.Dsl.Dsl,
  module Control.Dsl.Return,
  module Control.Dsl.Cont
) where

import Control.Dsl.Dsl hiding (cpsApply)
import Control.Dsl.Return (return, fail)
import Control.Dsl.Cont (when, unless, guard)

import Control.Dsl.State.State -- For orphan instances

import qualified Control.Monad -- For resolving haddock links
import qualified Control.Dsl.PolyCont -- For resolving haddock links
import qualified Control.Dsl.Cont -- For resolving haddock links
import qualified Control.Dsl.Return -- For resolving haddock links
import qualified Control.Dsl.Empty -- For resolving haddock links
import qualified Control.Dsl.Yield -- For resolving haddock links
import qualified Control.Dsl.State.Get -- For resolving haddock links
import qualified Control.Dsl.State.Put -- For resolving haddock links

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
-- >>> import Control.Dsl.State
-- >>> import Data.Void
-- >>> import System.IO
-- >>> import System.IO.Temp
