{-# LANGUAGE TypeOperators, FlexibleContexts, LambdaCase, RankNTypes #-}
module RunTimeAndConsole where

import Free
import SimpleOpenUnion
import TimeM
import ConsoleIO
import Prelude as IO
import Control.Monad

getCurrentTime :: IO CurrentTime
getCurrentTime = undefined

interpretConsoleIO :: Console a -> IO a
interpretConsoleIO (ReadLine f) = f <$> getLine
interpretConsoleIO (WriteLine l a) = putStrLn l >> pure a

interpretTimeIO :: Time a -> IO a
interpretTimeIO (GetTime f) = f <$> getCurrentTime

interpretSum :: (forall a . f a -> m a) -> (forall a . g a -> m a) -> (f :+: g) a -> m a
interpretSum handleF _ (Inl f) = handleF f
interpretSum _ handleG (Inr g) = handleG g

runTimeAndConsole :: Free (Time :+: Console) a -> IO a
runTimeAndConsole = iterM $ join . interpretSum interpretTimeIO interpretConsoleIO
