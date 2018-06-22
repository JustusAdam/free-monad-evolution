{-# LANGUAGE RankNTypes, GADTs #-}

data Coyoneda f a where
  Coyoneda :: (b -> a) -> f b -> Coyoneda f a

instance Functor (Coyoneda a) where
  fmap f (Coyoneda g v) = Coyoneda (f . g) v
