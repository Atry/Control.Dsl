{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Control.Dsl.Cont where

import Control.Dsl.PolyCont
import Prelude hiding ((>>), (>>=), return)


-- ==== __Examples__

-- >>> :set -XTypeOperators
-- >>> :set -XRebindableSyntax
-- >>> import Prelude hiding ((>>), (>>=), return)
-- >>> import Control.Dsl
-- >>> import Control.Dsl.Yield
-- >>> import Control.Dsl.Empty
-- >>> :{
-- f :: IO () !! [Integer] !! [String] !! [Double]
-- f = do
--   Yield "foo"
--   Yield 0.5
--   Yield 42
--   empty
-- :}
-- -}

-- | A type alias to 'Cont' for a deeply nested delimited continuation.
type r !! a = Cont r a

-- ! A delimited continuation that can be used in a @do@ block.
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

when True (Cont k) = Cont k
when False _ = Cont $ \f -> f ()

unless True _ = Cont $ \f -> f ()
unless False (Cont k) = Cont k

instance {-# OVERLAPS #-} PolyCont k r a => PolyCont k (Cont r b) a where
  runPolyCont k f = Cont $ \g -> runPolyCont k $ \a -> runCont (f a) g