module MyTypes.MyTree where

data MyTree a = Leaf a | Node a (MyTree a) (MyTree a) deriving (Show, Eq)

instance Foldable MyTree where
  foldr f y (Leaf x) = f x y
  -- foldr f y (Node x l r) =
  --   let rt = foldr f y r
  --       lt = foldr f y l
  --   in f x
  foldr f y (Node x l r) =
    let rt = foldr f y r
        lt = foldr f rt l
     in f x lt

instance Functor MyTree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance Applicative MyTree where
  pure a = Leaf a
  (<*>) (Leaf f) (Leaf x) = Leaf (f x)
  (<*>) (Leaf f) (Node x l r) = (Node (f x) (fmap f l) (fmap f r))
  (<*>) (Node f l r) (Leaf x) = Node (f x) (l <*> Leaf x) (r <*> Leaf x)

-- ??????????????
  -- (<*>) (Node f l r) (Node x lx rx) = Node (f x) (l <*> lx) (r <*> rx) 
  (<*>) (Node f l r) (Node x lx rx) = Node (f x) (l <*> Node x lx rx) (r <*> Node x lx rx)

-- For test
-- Дерево функций
fTree :: MyTree (Int -> Int)
fTree = Node (+ 1) (Leaf (* 2)) (Leaf (\x -> x - 3))

-- Дерево значений
xTree :: MyTree Int
xTree = Node 10 (Leaf 5) (Node 8 (Leaf 1) (Leaf 2))

testA :: MyTree Int
testA = fTree <*> xTree

-- fmap, (<$)
{-
    ------------- fmap:

    ghci> fmap (*2) (Node 1 (Leaf 2) (Leaf 10))
    Node 2 (Leaf 4) (Leaf 20)

    ghci> fmap (+5) (Leaf 5)
    Leaf 10

    ------------- (<$):

    ghci> 100 <$ (Node 1 (Leaf 2) (Leaf 10))
    Node 100 (Leaf 100) (Leaf 100)

    ghci> (Leaf 100) <$ (Node 1 (Leaf 2) (Leaf 10))
    Node (Leaf 100) (Leaf (Leaf 100)) (Leaf (Leaf 100))
-}

-- pure, (<*>), liftA2, (*>), (<*)
{-
    ------------- pure:

    ghci> pure 33 :: MyTree Int
    Leaf 33

    ghci> pure "foo" :: MyTree String
    Leaf "foo"

    ------------- (<*>):

    ghci> Leaf (+1) <*> (Node 1 (Leaf 2) (Leaf 4))
    Node 2 (Leaf 3) (Leaf 5)

    ghci> Leaf (*0) <*> (Node 1 (Node 10 (Leaf 11) (Leaf 12)) (Leaf 3))
    Node 0 (Node 0 (Leaf 0) (Leaf 0)) (Leaf 0)

    ------------- liftA2:

    ghci> liftA2 (+) (Node 1 (Leaf 2) (Leaf 3)) (Node 10 (Leaf 20) (Leaf 30))
    Node 11 (Node 12 (Leaf 22) (Leaf 32)) (Node 13 (Leaf 23) (Leaf 33))

    ghci> liftA2 (*) (Leaf 1) (Node 1 (Leaf 2) (Leaf 3))
    Node 1 (Leaf 2) (Leaf 3)

    ------------- (*>):

    ghci> (Node 5 (Leaf 6) (Leaf 7)) *> (Leaf 10)
    Node 10 (Leaf 10) (Leaf 10)

    ghci> (Node 5 (Leaf 6) (Leaf 7)) *> (Node 10 (Leaf 11) (Leaf 12))
    Node 10 (Node 10 (Leaf 11) (Leaf 12)) (Node 10 (Leaf 11) (Leaf 12))

    ------------- (<*):
    ghci> (Node 10 (Leaf 5) (Node 8 (Leaf 1) (Leaf 2))) <* (Node 100 (Leaf 10) (Leaf 20))
    Node 10 (Node 5 (Leaf 5) (Leaf 5)) (Node 8 (Node 1 (Leaf 1) (Leaf 1)) (Node 2 (Leaf 2) (Leaf 2)))

-}

-- fold, foldMap, foldr
{-
    ------------- foldr:
    ghci> foldr (+) 0 (Node 1 (Leaf 10) (Leaf 100))
    111

    ghci> foldr (+) 0 (Leaf 100)
    100

    ------------- fold:
    ghci> import Data.Foldable as FO

    ghci> fold (fmap (:[]) (Node 1 (Leaf 2) (Leaf 3)))
    [1,2,3]

    ghci> fold (Node "foo, " (Leaf "bar, ") (Leaf "bazz"))
    "foo, bar, bazz"

    ------------- foldMap:
    import Data.Monoid as M

    ghci> foldMap Sum (Node 1 (Leaf 2) (Leaf 3))
    Sum {getSum = 6}
-}
