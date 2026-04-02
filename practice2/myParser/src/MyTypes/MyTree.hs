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
  -- (<*>) (Node f l r) (Node x lx rx) = Node (f x) (l <*> lx) (r <*> rx) ??????????????
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
