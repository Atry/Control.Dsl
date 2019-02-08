{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE GADTs #-}

module Control.Dsl.State.State where

import Data.Void
import Control.Dsl.Empty
import Control.Dsl.Return
import Control.Dsl.PolyCont
import Prelude hiding ((>>), (>>=), return, fail)

{- |
The type that holds states, which is defined as a plain function.

==== __Examples__

>>> :set -XFlexibleContexts
>>> :set -XTypeApplications
>>> :set -XRebindableSyntax
>>> import Prelude hiding ((>>), (>>=), return, fail)
>>> import Control.Dsl
>>> import Control.Dsl.Cont
>>> import Control.Dsl.Shift
>>> import Control.Dsl.State
>>> import Data.Sequence (Seq, (|>))
>>> import qualified Data.Sequence
>>> import Data.Foldable

The following @append@ function 'Control.Dsl.State.Get's a @Seq String@ state,
appends @s@ to the @Seq@,
and 'Control.Dsl.State.Put's the new @Seq@ as the updated state.

>>> :{
append s = do
  buffer <- Get @(Seq String)
  toCont $ Put $ buffer |> s
:}

@append@ returns a 'Control.Dsl.Cont.Cont',
which can be used in other @do@ blocks.

A @formatter@ @append@s 'String' to its internal buffer,
and 'Control.Dsl.return' the concatenated buffer.

>>> :{
formatter = do
  append "x="
  d <- Get @Double
  append $ show d
  append ",y="
  i <- Get @Integer
  append $ show i
  buffer <- Get @(Seq String)
  return $ concat buffer
:}

>>> x = 0.5 :: Double
>>> y = 42 :: Integer
>>> initialBuffer = Data.Sequence.empty :: Seq String
>>> formatter x y initialBuffer :: String
"x=0.5,y=42"

Note that @formatter@ accepts arbitrary order of the parameters,
or additional unused parameters.

>>> formatter "unused parameter" initialBuffer y x :: String
"x=0.5,y=42"
-}
type State = (->)

{- | The 'PolyCont' derivation rule for any keywords in a 'State' @do@ block.

This derivated instance provide the ability similar
to @StateT@ or @ReaterT@ monad transformers.
-}
instance {-# OVERLAPS #-} PolyCont k r a => PolyCont k (State s r) a where
  runPolyCont k f s = runPolyCont k (`f` s)

instance {-# OVERLAPS #-} PolyCont k r Void => PolyCont k (State s r) Void where
  runPolyCont k _ _ = runPolyCont k absurd

instance {-# OVERLAPS #-} PolyCont Empty r Void => PolyCont Empty (State s r) Void where
  runPolyCont k _ _ = empty

instance PolyCont (Return r) (State s r) Void where
  runPolyCont (Return r) _ _ = r
