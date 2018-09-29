{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Control.Dsl.Return where

import Prelude (Applicative, id, pure, ($))
import Control.Dsl.Internal

newtype Return a b = Return a

instance {-# OVERLAPPABLE #-} Dsl (Return a) b a where
  Return a >>= _ = a

instance {-# INCOHERENT #-} (Applicative m, Dsl (Return a) d d) => Dsl (Return a) b (m d) where
  Return x >>= _ = pure $ Return x >>= id

return :: forall d a. Dsl (Return a) d d => a -> d
return x = Return x >>= id