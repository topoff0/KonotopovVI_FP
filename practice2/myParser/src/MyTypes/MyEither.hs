module MyTypes.MyEither where

data MyEither a b = MyLeft a | MyRight b deriving (Show, Eq)

instance Foldable (MyEither a) where
  foldr _ z (MyLeft _) = z
  foldr f z (MyRight y) = f y z

-- | @since base-4.9.0.0
-- instance Semigroup (Either a b) where
--     Left _ <> b = b
--     a      <> _ = a

-- Какая реализация верная: прокидывает MyLeft дальше (как ошибку) или берем первое верное значение (MyRight)?
-- instance Semigroup (MyEither a b) where
--   (<>) (MyLeft _) b = b
--   (<>) a _ = a

instance (Semigroup a) => Semigroup (MyEither b a) where
  (<>) (MyLeft x) _ = MyLeft x
  (<>) _ (MyLeft x) = MyLeft x
  (<>) (MyRight x) (MyRight y) = MyRight (x <> y)

instance Functor (MyEither a) where
  fmap _ (MyLeft x) = MyLeft x
  fmap f (MyRight y) = MyRight (f y)

-- | @since base-3.0
-- instance Applicative (Either e) where
--     pure          = Right
--     Left  e <*> _ = Left e
--     Right f <*> r = fmap f r
instance Applicative (MyEither a) where
  pure x = MyRight x
  (<*>) (MyLeft a) _ = MyLeft a
  (<*>) (MyRight f) r = fmap f r

-- Можно явно прописать все варианты (по факту то, что делает fmap)
-- (<*>) (MyLeft e) _ = MyLeft e
-- (<*>) _ (MyLeft e) = MyLeft e
-- (<*>) (MyRight f) (MyRight x) = MyRight (f x)
