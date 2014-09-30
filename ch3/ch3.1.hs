-- Exercise 1
data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

fromList' :: List a -> [a]
fromList' Nil = []
fromList' (Cons x y) = x : fromList' y

-- Exercise 2
{-
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
-}

data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)

data Tree' a = Node' (Maybe a) (Maybe (Tree' a)) (Maybe (Tree' a))
               deriving (Show)
