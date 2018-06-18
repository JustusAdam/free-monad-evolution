{-# LANGUAGE RankNTypes, GADTs, TypeOperators, FlexibleContexts #-}

module Reader where

import Control.Monad
import Free
import Interpret

data Reader e a =
  Ask (e -> a)

ask :: (Reader e :<: effs, Functor effs) => Free effs e
ask = Impure $ inj $ Ask pure

interpretReader :: e -> Free (Reader e :+: effs) a -> Free effs a
interpretReader env = interpret $ \(Ask cont) -> pure $ cont env
