newtype StateT s m a = StateT
  { runStateT :: s -> m (a, s)
  }

instance Monad m
  => Monad (StateT s m) where
  pure a = StateT $ \s -> pure $ (a, s)
  v >>= f =
    StateT $ \s -> do
      (a, s') <- runStateT v s
      runStateT (f a) s'

class MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

instance Monad m
  => MonadState s (StateT s m) where
  get = StateT $ \s -> pure (s, s)
  put a = StateT $ const $ pure ((), a)
