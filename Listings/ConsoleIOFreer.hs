{-# LANGUAGE GADTs, LambdaCase #-}

module ConsoleIOFreer where

data ConsoleIO a where
  ReadLine :: ConsoleIO String
  WriteLine :: String -> ConsoleIO ()

runConsoleIO ::
     MonadIO (Eff effs) => Eff (ConsoleIO :+: effs) a -> Eff effs a
runConsoleIO =
  interpret $ \case
    ReadLine -> liftIO getLine
    WriteLine line -> liftIO $ putStrLn line
