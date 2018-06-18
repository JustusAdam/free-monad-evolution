module Free where

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> other = fmap f other
  Impure eff <*> other = Impure $ fmap (<*> other) eff

data Free f a
  = Impure (f (Free f a))
  | Pure a

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Impure eff) = Impure $ fmap (fmap f) eff

instance Functor f => Monad (Free f) where
  return = Pure
  Pure a >>= cont = cont a
  Impure fa >>= cont = Impure $ fmap (>>= cont) fa

iter :: Functor f => (f a -> a) -> Free f a -> a
iter _ (Pure a) = a
iter handle (Impure fa) = handle $ fmap (iter handle) fa

iterM :: (Monad m, Functor f) => (f (m a) -> m a) -> Free f a -> m a
iterM handle (Pure a) = pure a
iterM handle (Impure fa) = handle $ fmap (iterM handle) fa
