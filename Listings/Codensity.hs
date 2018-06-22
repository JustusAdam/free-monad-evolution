{-# LANGUAGE RankNTypes, GADTs #-}

module Codensity where

instance Functor (Codensity m)

instance Applicative (Codensity m)

newtype Codensity m a = Codensity
  { runCodensity ::
      forall w. (a -> m w) -> m w
  }

instance Monad (Codensity m) where
  return v = Codensity ($ v)
  c >>= f = Codensity $ \k ->
    runCodensity c $ \v ->
    runCodensity (f v) k

lowerCodensity :: Applicative f
               => Codensity f a -> f a
lowerCodensity (Codensity f) = f pure
