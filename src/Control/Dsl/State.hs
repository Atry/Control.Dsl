{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{- |
Description : Mutable variables

This module provides keywords to 'Put' and 'Get' the value of multiple mutable variables in a @do@ block.
-}
module Control.Dsl.State (
  module Control.Dsl.State.Put,
  module Control.Dsl.State.Get,
  module Control.Dsl.State.State,
) where

import Control.Dsl.State.Put
import Control.Dsl.State.Get
import Control.Dsl.State.State
