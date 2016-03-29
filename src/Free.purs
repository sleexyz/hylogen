module Free where

import Prelude

data Free f a = Free (f (Free f a))
              | Pure a

out :: forall f a. Free f a -> f (Free f a)
out (Free fx) = fx


instance showFree :: (Functor f, Show (f (Free f a)), Show a) => Show (Free f a) where
  show (Pure x) = show x
  show (Free fx) = show fx


instance functorFree :: (Functor f) => Functor (Free f) where
  map g (Free fx) = Free (map (map g) fx)
  map g (Pure x) = Pure (g x)


instance applyFree :: (Functor f) => Apply (Free f) where
  apply (Pure g) x = map g x
  apply (Free fg) x= Free (map (\y -> apply y x) fg)

instance applicativeFree :: (Functor f) => Applicative (Free f) where
  pure x = Pure x

instance bindFree :: (Functor f) => Bind (Free f) where
  bind (Free x) kf = Free (map (\y -> bind y kf) x)
  bind (Pure x) kf = kf x

instance monadFree :: (Functor f) => Monad (Free f)


liftF :: forall f r. (Functor f) => f r -> Free f r
liftF fr = Free (Pure <$> fr)

