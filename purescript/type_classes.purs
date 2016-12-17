
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b


class Functor f <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b


instance functorMaybe :: Functor Maybe where
  map f (Just a) = Just (f a)
  map f Nothing = Nothing

instance applyMaybe :: Apply Maybe where
  apply (Just f) (Just x) = Just (f x)
  apply _        _        = Nothing

