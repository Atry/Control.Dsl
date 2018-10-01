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
>>> import Prelude hiding ((>>), (>>=), return)
>>> import Control.Dsl
>>> import Control.Dsl.Yield

>>> :{
f = do
  Yield "foo"
  Yield "bar"
  return "baz"
:}

>>> :type f
f :: (Dsl (Yield [Char]) r (),
      Dsl (Control.Dsl.Return.Return [Char]) r Data.Void.Void) =>
     r

>>> f :: [String]
["foo","bar","baz"]

>>> import qualified Prelude
>>> :{
instance Dsl (Yield String) (IO a) () where
  cpsApply (Yield a) = (Prelude.>>=) (putStrLn $ "Yield " ++ a)
:}

>>> f :: IO String
Yield foo
Yield bar
"baz"
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