{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
  TypeOperators #-}

module SimpleOpenUnion where

data (f :+: g) a
  = Inl (f a)
  | Inr (g a)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl fa) = Inl $ fmap f fa
  fmap f (Inr ga) = Inr $ fmap f ga

class sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance f :<: f where
  inj = id
  prj = Just

instance f :<: (f :+: g) where
  inj = Inl
  prj (Inl f) = Just f
  prj _ = Nothing

instance {-# OVERLAPPABLE #-}
  (f :<: sup) => f :<: (g :+: sup) where
  inj = Inr . inj
  prj (Inr sum) = prj sum
  prj _ = Nothing
