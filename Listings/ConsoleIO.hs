{-# LANGUAGE LambdaCase #-}

module ConsoleIO where

import Free
import Prelude as IO

data Console a
  = ReadLine (String -> a)
  | WriteLine String a

instance Functor Console where
  fmap f (ReadLine cont) =
    ReadLine $ fmap f cont
  fmap f (WriteLine str a) =
    WriteLine str $ f a

type ConsoleM = Free Console

readLine :: ConsoleM String
readLine = Impure (ReadLine pure)

writeLine :: String -> ConsoleM ()
writeLine str =
  Impure (WriteLine str (pure ()))

interpretIO :: ConsoleM a -> IO a
interpretIO =
  iterM $ \case
    ReadLine f -> getLine >>= f
    WriteLine line cont ->
      putStrLn line >> cont

interpretPure :: [String] -> ConsoleM a
              -> (a, [String])
interpretPure ls = go ls []
  where
    go _ outLines (Pure v) =
      (v, reverse outLines)
    go inLines outLines (Impure eff) =
      case eff of
        ReadLine cont ->
          go (tail inLines) outLines
            $ cont (head inLines)
        WriteLine str cont ->
          go inLines (str : outLines) cont
