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
     (f :<: sum, Functor f, Functor sum)
  => (forall b. f b -> Free sum b)
  -> Free sum a
  -> Free sum a
interpose _ (Pure a) = Pure a
interpose handler (Impure eff) =
  case prj eff of
    Nothing -> Impure eff
    Just f -> join $ handler $ fmap (interpose handler) f

addTimestamp ::
     (Functor sum, Time :<: sum, Console :<: sum)
  => Free sum a
  -> Free sum a
addTimestamp =
  interpose $ \case
    WriteLine line cont -> do
      ts <- getTime
      writeLine (show ts ++ line)
      pure cont
    other -> Impure $ inj $ fmap pure other

escapeInput ::
     (Functor sum, Console :<: sum)
  => (String -> String)
  -> Free sum a
  -> Free sum a
escapeInput sanitize =
  interpose $ \case
    ReadLine cont -> do
      line <- readLine
      pure $ cont (sanitize line)
    other -> Impure $ inj $ fmap pure other
