handle_relay :: Typeable t
             => Union (t |> r) v
             -> (v -> Eff r a)
             -> (t v -> Eff r a)
             -> Eff r a
handle_relay u loop h =
  case decomp u of
    Right x -> h x
    Left u -> send (\k -> fmap k u) >>= loop
