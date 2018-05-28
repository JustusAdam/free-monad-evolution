{-# LANGUAGE TypeOperators, FlexibleContexts, Rank2Types #-}

module Interpret where

import Control.Monad
import Data.Functor.Identity
import Free
import SimpleOpenUnion

interpret ::
     (Functor sum, Functor f)
  => (forall a. f a -> Free sum a)
  -> Free (f :+: sum) a
  -> Free sum a
interpret _ (Pure a) = pure a
interpret handle (Impure (Inr others)) =
  Impure $ fmap (interpret handle) others
interpret handle (Impure (Inl eff)) =
  join $ handle $ fmap (interpret handle) eff

reinterpret ::
     (Functor f, Functor g, Functor sum)
  => (forall a. f a -> Free (g :+: sum) a)
  -> Free (f :+: sum) b
  -> Free (g :+: sum) b
reinterpret _ (Pure a) = pure a
reinterpret handle (Impure (Inr others)) =
  Impure $ Inr $ fmap (reinterpret handle) others
reinterpret handle (Impure (Inl eff)) =
  join $ handle $ fmap (reinterpret handle) eff

run :: Free Identity a -> a
run = iter runIdentity

runM :: Monad m => Free m a -> m a
runM = iterM join
