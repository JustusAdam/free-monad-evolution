{-# LANGUAGE GADTs #-}

data TList a b where
  Nil :: TList a a
  (><) :: (a -> w) -> TList w b -> TList a b

toFun :: TList a b -> (a -> b)
toFun Nil = id
toFun (f >< l) = f . toFun l
