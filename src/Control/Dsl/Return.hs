{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Dsl.Return where

import Control.Dsl.Internal

newtype Return a b = Return a

instance {-# OVERLAPPABLE #-} Dsl (Return a) b a where
  (>>=) (Return a) _ = a

instance {-# INCOHERENT #-} (Applicative m, Dsl (Return a) d d) => Dsl (Return a) b (m d) where
  (>>=) (Return x) _ = pure $ Return x Control.Dsl.Internal.>>= id

return x = Return x Control.Dsl.Internal.>>= id