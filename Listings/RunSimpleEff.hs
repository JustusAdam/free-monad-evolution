{-# LANGUAGE TypeOperators, FlexibleContexts, RankNTypes #-}

module RunSimpleEff where

runLayer ::
     Monad m
  => (forall a. f a -> m a)
  -> Free (f :+: other) a
  -> m (Free other a)
runLayer _ (Pure a) = pure a
runLayer _ (Impure (Inr otherEff)) = Impure otherEff
runLayer handle (Impure (Inl eff)) = handle eff >>= runLayer handle
