module MyTypes.MyMaybe where

data MyMaybe a = MyNothing | MyJust a deriving (Show, Eq)

instance (Semigroup a) => Semigroup (MyMaybe a) where
  (<>) MyNothing b = b
  (<>) a MyNothing = a
  (<>) (MyJust a) (MyJust b) = MyJust (a <> b)

instance (Semigroup a) => Monoid (MyMaybe a) where
  mempty = MyNothing

instance Functor MyMaybe where
  fmap _ MyNothing = MyNothing
  fmap f (MyJust a) = MyJust (f a)

instance Applicative MyMaybe where
  pure a = MyJust a
  (<*>) MyNothing _ = MyNothing
  (<*>) _ MyNothing = MyNothing
  (<*>) (MyJust f) (MyJust a) = MyJust (f a)

instance Foldable MyMaybe where
  foldr _ x MyNothing = x
  foldr f x (MyJust b) = f b x

-- (<>), sconcat, stimes
{-
    ------------- (<>):

        ghci> MyJust "foo" <> MyJust ", bar"
        MyJust "foo, bar"

        ghci> (<>) (MyJust [1,2]) (MyJust [3])
        MyJust [1,2,3]

        ghci> MyNothing <> MyJust "bazz"
        MyJust "bazz"

        ghci> (<>) MyNothing MyNothing
        MyNothing

    ------------- sconcat:
        ghci> import Data.Semigroup as S
        ghci> import Data.List.NonEmpty as NE

        ghci> sconcat (MyJust "a" :| [MyJust "b", MyJust "c"])
        MyJust "abc"

        ghci> sconcat (MyNothing :| [MyJust "foo", MyNothing])
        MyJust "foo"

    ------------- stimes:

        ghci> stimes 3 (MyJust "ha")
        MyJust "hahaha"

        ghci> stimes 10 MyNothing
        MyNothing
-}

-- mappend, mconcat
{-
    ------------- mappend:

        ghci> mappend (MyJust "foo") (MyJust ", bar")
        MyJust "foo, bar"

        ghci> mappend MyNothing (MyJust "baz")
        MyJust "baz"

        ghci> mappend MyNothing MyNothing
        MyNothing

    ------------- mconcat:

        ghci> mconcat [MyJust "a", MyJust "b", MyJust "c"]
        MyJust "abc"

        ghci> mconcat [MyNothing, MyJust "foo", MyNothing, MyNothing]
        MyJust "foo"
-}

-- fmap, (<$)
{-
    ------------- fmap:

        ghci> fmap (+1) MyNothing
        MyNothing

        ghci> fmap (+1) (MyJust 4)
        MyJust 5

    ------------- (<$):

        ghci> 10 <$ MyNothing
        MyNothing

        ghci> 10 <$ MyJust "hello"
        MyJust 10
-}

-- pure, (<*>), liftA2, (*>), (<*)
{-
    ------------- pure:

        ghci> pure 5 :: MyMaybe Int
        MyJust 5

        ghci> pure "ab" :: MyMaybe String
        MyJust "ab"

    ------------- (<*>):

        ghci> (MyJust (*2)) <*> (MyJust 5)
        MyJust 10

        ghci> MyNothing <*> (MyJust 5)
        MyNothing

    ------------- liftA2:

        ghci> liftA2 (+) (MyJust 2) (MyJust 3)
        MyJust 5

        ghci> liftA2 (+) MyNothing (MyJust 3)
        MyNothing

    ------------- (*>):

        ghci> (MyJust "foo") *> (MyJust "bar")
        MyJust "bar"

        ghci> (MyJust "foo") *> MyNothing
        MyNothing

        ghci> MyNothing *> (MyJust "bar")
        MyNothing

    ------------- (<*):

        ghci> (MyJust "bazz") <* (MyJust "bar")
        MyJust "bazz"

        ghci> MyNothing <* (MyJust "bar")
        MyNothing
-}

-- fold, foldMap, foldr
{-
    ------------- foldr:

        ghci> foldr (+) 0 MyNothing
        0

        ghci> foldr (+) 0 (MyJust 5)
        5

    ------------- fold:
        ghci> import Data.Foldable as FO

        fold (MyJust "hi")
        "hi"

        ghci> fold MyNothing
        ()

    ------------- foldMap:
        ghci> import Data.Monoid as M

        ghci> foldMap Sum MyNothing
        Sum {getSum = 0}

        ghci> foldMap (Sum . (+1)) $ MyJust 1
        Sum {getSum = 2}
-}
