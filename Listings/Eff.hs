{-# LANGUAGE RankNTypes #-}


data F r a = P a | I (Union r (F r a))

-- Same as `Codensity (F r)`
newtype Eff r a = Eff { runEff :: forall w . (a -> F r w) -> F r w }

instance Monad (Eff r) where
  return = pure

  m >>= cont = Eff $ \f -> runEff m $ \v -> runEff (cont v) f

instance Functor (Eff r) where
  fmap g (Eff f) = Eff $ \k -> f (k . g)

instance Applicative (Eff r) where
  pure v = Eff ($ v)

  Eff mf <*> Eff mv = Eff $ \k -> mf $ \f -> mv (k . f)
