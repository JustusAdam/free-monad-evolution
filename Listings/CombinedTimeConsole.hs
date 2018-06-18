reportCurrentTime :: (Console :<: f, Time :<: f) => Free f ()
reportCurrentTime = getTime >>= writeLine . show
