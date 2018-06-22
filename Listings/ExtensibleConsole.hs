{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module ExtensibleConsole where

import ConsoleIO hiding (readLine, writeLine)
import Free
import SimpleOpenUnion

writeLine :: (Console :<: effs, Functor effs)
          => String -> Free effs ()
writeLine str =
  Impure $ inj $ WriteLine str (pure ())

readLine :: (Console :<: effs, Functor effs)
         => Free effs String
readLine = Impure $ inj $ ReadLine pure
