{-# LANGUAGE RankNTypes, GADTs #-}

data Coyoneda f a where
  Coyoneda :: (b -> a) -> f b -> Coyoneda f a

hoistCoyoneda ::
     (forall a. f a -> g a) -> Coyoneda f b -> Coyoneda g b
hoistCoyoneda trans (Coyoneda alter f) = Coyoneda alter (trans f)

lowerCoyoneda :: Functor f => Coyoneda f a -> f a
lowerCoyoneda = lowerWith fmap

lowerWith ::
     (forall a b. (a -> b) -> f a -> f b) -> Coyoneda f c -> f c
lowerWith map (Coyoneda alter f) = map alter f
