{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module TimeM where

import Free
import SimpleOpenUnion

data CurrentTime =
  CurrentTime
  deriving (Show)

data Time a =
  GetTime (CurrentTime -> a)

instance Functor Time where
  fmap f (GetTime g) = GetTime (f . g)

getTime :: (Functor f, Time :<: f) => Free f CurrentTime
getTime = Impure $ inj $ GetTime pure
