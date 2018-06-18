{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module ExtensibleConsole where

import ConsoleIO hiding (readLine, writeLine)
import Free
import SimpleOpenUnion

writeLine :: (Console :<: f, Functor f) => String -> Free f ()
writeLine str = Impure $ inj $ WriteLine str (pure ())

readLine :: (Console :<: f, Functor f) => Free f String
readLine = Impure $ inj $ ReadLine pure
