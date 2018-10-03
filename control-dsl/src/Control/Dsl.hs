{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Dsl(
  module Control.Dsl.Dsl,
  module Control.Dsl.Return,
  module Control.Dsl.Empty,
  module Control.Dsl.Cont
) where

import Control.Dsl.Dsl hiding (cpsApply)
import Control.Dsl.Return (return)
import Control.Dsl.Cont (when, unless)
import Control.Dsl.Empty (guard)
