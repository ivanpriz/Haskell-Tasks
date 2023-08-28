instance Functor SomeType where
    fmap f x = let helper = \f unpacked -> return (f unpacked) in x >>= helper f
