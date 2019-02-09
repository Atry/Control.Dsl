
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Description : Delimited continuations
-}
module Control.Dsl.Shift where

import           Data.Void
import           Control.Dsl.PolyCont
import           Prelude                 hiding ( (>>)
                                                , (>>=)
                                                , return
                                                , fail
                                                )

-- | A keyword to extract the value of a CPS function .
newtype Shift r' r a = Shift ((a -> r') -> r')

instance PolyCont (Shift r) r a where
  runPolyCont (Shift k) = k
