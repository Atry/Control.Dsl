{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Control.Dsl.Internal.Maybe where

import Prelude hiding ((>>), (>>=), return)
import Control.Dsl.Return
import Control.Dsl.Internal

{- |
This instance automatically lifts a 'Maybe' to other domains,
similar to @MaybeT@ but simpler.

=== Examples

Suppose I want to create a command line tool,
which read a file and parse it as integers, then returns the sum.

>>> :set -XTypeApplications
>>> :set -XRebindableSyntax
>>> import Prelude hiding ((>>), (>>=), return)
>>> import Control.Dsl
>>> import Text.Read
>>> import System.IO
>>> :{
calculator :: Handle -> IO (Maybe Integer)
calculator h = do
  putStrLn "Input the first number..."
  line1 <- hGetLine h
  number1 <- readMaybe @Integer line1
  putStrLn "Input the second number..."
  line2 <- hGetLine h
  number2 <- readMaybe @Integer line2
  return (number1 + number2)
:}


When the input is correct, it should compute the sum of the input.

>>> import System.IO.Temp
>>> :{
withSystemTempFile "tmp" $ \_ -> \h -> do
  hPutStrLn h "40"
  hPutStrLn h "2"
  hSeek h AbsoluteSeek 0
  calculator h
:}
Input the first number...
Input the second number...
Just 42

When the input is in wrong format,
it should return an @IO Nothing@.

>>> import System.IO.Temp
>>> :{
withSystemTempFile "tmp" $ \_ -> \h -> do
  hPutStrLn h "not a number"
  hPutStrLn h "2"
  hSeek h AbsoluteSeek 0
  calculator h
:}
Input the first number...
Nothing

Note that the same functionality implemented in @MaybeT@ monad transformers
would be much complicated than the above example,
due to manually @lift@ing 'Maybe' and 'IO' to a common type.
-}
instance Dsl (Return (Maybe a)) d d => Dsl Maybe a d where
  Nothing >>= f = return @(Maybe a) Nothing
  Just a >>= f = f a
