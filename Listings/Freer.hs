{-# LANGUAGE GADTs #-}

data Freer f a where
  Pure :: a -> Freer f a
  Impure :: f x -> (x -> Freer f a)
         -> Freer f a
