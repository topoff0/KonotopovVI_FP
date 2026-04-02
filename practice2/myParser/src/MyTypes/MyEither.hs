module MyTypes.MyEither where

data MyEither a b = MyLeft a | MyRight b deriving (Show, Eq)

instance Foldable (MyEither a) where
  foldr _ z (MyLeft _) = z
  foldr f z (MyRight y) = f y z

-- | @since base-4.9.0.0
-- instance Semigroup (Either a b) where
--     Left _ <> b = b
--     a      <> _ = a

-- Какая реализация верная: прокидываем MyLeft дальше (как ошибку) или берем первое верное значение (MyRight)?
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

-- (<>), sconcat, stimes
{-
    ------------- (<>):

    ghci> ((<>) (MyRight "foo") (MyRight ", bar")) <> (MyRight ", bazz")
    MyRight "foo, bar, bazz"

    ghci> ((<>) (MyRight "foo") (MyRight ", bar")) <> (MyLeft  ", bazz")
    MyLeft ", bazz"

    ------------- sconcat:
    ghci> import Data.Semigroup as S
    ghci> import Data.List.NonEmpty as NE

    ghci> sconcat $ (MyRight "a") :| [(MyRight "b"), (MyRight "c")]
    MyRight "abc"

    ghci> sconcat $ (MyRight "a") :| [(MyRight "b"), (MyLeft "c")]
    MyLeft "c"

    ------------- stimes:

    ghci> stimes 3 (MyRight "foo ")
    MyRight "foo foo foo "

    ghci> stimes 3 (MyLeft "foo ")
    MyLeft "foo "
-}

-- fmap, (<$)
{-
    ------------- fmap:

    ghci> fmap (*2) (MyRight 10)
    MyRight 20

    ghci> fmap (+2) (MyRight 10)
    MyRight 12

    ghci> fmap (+2) (MyLeft 10)
    MyLeft 10

    ------------- (<$):

    ghci> 10 <$ MyLeft 1
    MyLeft 1

    ghci> Right 10 <$ MyRight 1
    MyRight (Right 10)
-}

-- pure, (<*>), liftA2, (*>), (<*)
{-
    ------------- pure:
    ghci> pure 10 :: MyEither String Int
    MyRight 10

    ghci> pure "10" :: MyEither Int String
    MyRight "10"

    ------------- (<*>):

    ghci> MyRight (+1) <*> MyRight 5
    MyRight 6

    ghci> MyRight (+) <*> MyRight 5 <*> MyRight 5
    MyRight 10

    ghci> MyRight (+) <*> MyRight 5 <*> MyLeft 1
    MyLeft 1

    ------------- liftA2:
    ghci> liftA2 (+) (MyRight 2) (MyRight 10)
    MyRight 12

    ghci> liftA2 (+) (MyRight 2) (MyLeft 10)
    MyLeft 10

    ------------- (*>):

    ghci> MyRight 1 *> MyRight 100
    MyRight 100

    ghci> MyLeft 1 *> MyRight 100
    MyLeft 1

    ------------- (<*):

    ghci> MyRight 1 <* MyRight 100
    MyRight 1

    ghci> MyRight 1 <* MyLeft 100
    MyLeft 100
-}

-- fold, foldMap, foldr
{-
    ------------- foldr:
    ghci> foldr (+) 10 (MyRight 5)
    15

    ghci> foldr (+) 10 (MyLeft 5)
    10

    ------------- fold:
    ghci> import Data.Foldable

    ghci> fold (MyRight "foo")
    "foo"

    ghci> fold (MyLeft "foo")
    ()

    ------------- foldMap:
    ghci> import Data.Monoid as M

    ghci> foldMap Sum (MyRight 5)
    Sum {getSum = 5}

    ghci> foldMap Sum (MyLeft 5)
    Sum {getSum = 0}

    ghci> foldMap (:[]) (MyRight "foo")
    ["foo"]

    ghci> foldMap (:[]) (MyRight 'f')
    "f"
-}
