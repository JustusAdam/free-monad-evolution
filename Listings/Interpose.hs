{-# LANGUAGE RankNTypes, TypeOperators, LambdaCase,
  FlexibleContexts #-}

module Interpose where

import ConsoleIO hiding (readLine, writeLine)
import Control.Monad
import ExtensibleConsole
import Free
import SimpleOpenUnion
import TimeM

interpose ::
     (f :<: effs, Functor f, Functor effs)
  => (forall b. f b -> Free effs b)
  -> Free effs a
  -> Free effs a
interpose _ (Pure a) = Pure a
interpose handler (Impure eff) =
  case prj eff of
    Nothing -> Impure eff
    Just f ->
      join $ handler $ fmap (interpose handler) f

addTimestamp :: ( Functor effs
                , Time :<: effs
                , Console :<: effs)
     => Free effs a -> Free effs a
addTimestamp =
  interpose $ \case
    WriteLine line cont -> do
      ts <- getTime
      writeLine (show ts ++ line)
      pure cont
    other -> Impure $ inj $ fmap pure other

escapeInput ::
     (Functor effs, Console :<: effs)
  => (String -> String)
  -> Free effs a -> Free effs a
escapeInput sanitize =
  interpose $ \case
    ReadLine cont -> do
      line <- readLine
      pure $ cont (sanitize line)
    other -> Impure $ inj $ fmap pure other
