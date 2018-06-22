data Union r v where
  Union :: (Functor t, Typeable t)
        => t v -> Union r v

inj :: (Typeable t, Functor t, t :<: r)
    => t a -> Union r a
inj = Union

prj :: (Typeable t, t :<: r)
    => Union r a -> Maybe (t a)
prj (Union v) = cast v
