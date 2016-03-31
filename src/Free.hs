{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Free where

import Prelude

-- data Free f a = Free (f (Free f a))
--               | Pure a

data Free f a where
  In   :: f (Free f a) -> Free f a
  Pure :: a -> Free f a


-- out :: forall f a. Free f a -> f (Free f a)
-- out (In fx) = fx


instance (Functor f, Show (f (Free f a)), Show a) => Show (Free f a) where
  show (Pure x) = show x
  show (In fx) = show fx


instance (Functor f) => Functor (Free f) where
  g `fmap` (In fx) = In ((g<$>) <$> fx)
  g `fmap` (Pure x) = Pure (g x)


instance (Functor f) => Applicative (Free f) where
  pure = Pure
  (Pure g) <*> x = g <$> x
  (In fg) <*> x = In $ (<*>x) <$> fg

instance (Functor f) => Monad (Free f) where
  (In x) >>= kf = In $ (>>=kf) <$> x
  (Pure x) >>= kf = kf x


liftF :: forall f a. (Functor f) => f a -> Free f a
liftF fa = In (Pure <$> fa)

