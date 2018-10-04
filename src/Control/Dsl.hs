{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

{- |

=== Examples

==== DSL model

>>> :set -XGADTs
>>> :set -XFlexibleContexts
>>> :set -XFlexibleInstances
>>> :set -XMultiParamTypeClasses
>>> :set -XUndecidableInstances
>>> :set -XTypeApplications
>>> :set -XRebindableSyntax
>>> import Prelude hiding ((>>), (>>=), return, fail)
>>> import qualified Prelude
>>> import Control.Dsl
>>> import Control.Dsl.PolyCont
>>> import Control.Dsl.Monadic
>>> import Control.Dsl.Cont
>>> import Control.Dsl.Yield
>>> import Control.Dsl.Return
>>> import Control.Dsl.Empty
>>> import Data.Void
>>> import System.IO
>>> import System.IO.Temp

>>> data GetLine r a where GetLine :: GetLine r String
>>> data MaxLengthConfig r a where MaxLengthConfig :: MaxLengthConfig r Int
>>> data PutStrLn r a where PutStrLn :: String -> PutStrLn r ()

==== DSL script

>>> :{
dslScript = do
  maxLength <- MaxLengthConfig
  line1 <- GetLine
  line2 <- GetLine
  when (length line1 + length line2 > maxLength) $ do
    fail "The input is too long"
  PutStrLn ("The input is " ++ line1 ++ " and " ++ line2)
  return ()
:}

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
["(handled) user error (The input is too long)"]

>>> runCont (runScriptPurely 80 ["ONE_LINE"]) errorHandler
["(handled) user error (Pattern match failure in do expression at ..."]

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
