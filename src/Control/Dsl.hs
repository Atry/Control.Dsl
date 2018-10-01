{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}

module Control.Dsl(
  module Control.Dsl.Dsl,
  module Control.Dsl.Return,
  module Control.Dsl.Do,
  module Control.Dsl.Cont
) where

import Control.Dsl.Return (return)
import Control.Dsl.Dsl (Dsl)
import Control.Dsl.Cont (when)
import Control.Dsl.Do ((>>=), (>>))

-- Import modules that contains orphan instances
import Control.Dsl.Shift ()
-- import Control.Dsl.State ()