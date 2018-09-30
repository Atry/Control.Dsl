{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}

module Control.Dsl(
  module Control.Dsl.Internal.Dsl,
  Control.Dsl.Return.return
) where

import Control.Dsl.Return (return)
import Control.Dsl.Internal.Dsl

-- Import modules that contains orphan instances
import Control.Dsl.Cont ()
import Control.Dsl.State ()
import Control.Dsl.Internal.Maybe ()
import Control.Dsl.Internal.Monad ()
