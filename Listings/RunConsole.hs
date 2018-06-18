{-# LANGUAGE TypeOperators, FlexibleContexts, Rank2Types,
  DeriveFunctor #-}

module RunConsole where

import ConsoleIO
import Control.Arrow
import Control.Monad
import Data.Functor.Identity
import Free
import Interpret
import SimpleOpenUnion

data State s a
  = Get (s -> a)
  | Put s
        a
  deriving (Functor)

runState ::
     Functor sum => s -> Free (State s :+: sum) a -> Free sum (a, s)
runState s (Pure a) = pure (a, s)
runState s (Impure (Inr other)) = Impure $ fmap (runState s) other
runState s (Impure (Inl eff)) =
  case eff of
    Get cont -> runState s $ cont s
    Put s' cont -> runState s' cont

put :: (State s :<: sum, Functor sum) => s -> Free sum ()
put s = Impure $ inj $ Put s $ pure ()

get :: (State s :<: sum, Functor sum) => Free sum s
get = Impure $ inj $ Get pure

liftIO :: (IO :<: sum, Functor sum) => IO a -> Free sum a
liftIO = Impure . inj . fmap pure

interpretConsolePure ::
     [String] -> Free (Console :+: Identity) a -> (a, [String])
interpretConsolePure strs =
  run .
  fmap (second (reverse . snd)) . runState (strs, []) . reinterpret f
  where
    f :: Console a -> Free (State ([String], [String]) :+: Identity) a
    f (ReadLine cont) = do
      (inLines, outLines) <- get
      put (tail inLines, outLines :: [String])
      pure $ cont $ head inLines
    f (WriteLine str cont) = do
      (inLines, outLines) <- get
      put (inLines :: [String], str : outLines)
      pure $ cont

interpretConsoleIO :: Free (Console :+: IO) a -> IO a
interpretConsoleIO = runM . interpret f
  where
    f (ReadLine cont) = cont <$> liftIO getLine
    f (WriteLine str cont) = liftIO (putStrLn str) >> pure cont
