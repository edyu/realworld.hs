import Data.List (sortBy, delete)

-- Exercise 1 & 2
length' :: (Num a) => [t] -> a
length' (x:xs) = 1 + (length' xs)
length' []     = 0

-- Exercise 3
mean [] = 0
mean xs = (sum xs) / (fromIntegral (length xs))

-- Exercise 4
palindrome xs = xs ++ (reverse xs)

-- Exercise 5
ispalindrome xs | odd (length xs)  = a == init (reverse b)
                | even (length xs) = a == reverse b
             where mid    = (length xs) `div` 2
                   (a, b) = splitAt mid xs

-- Exercise 6
sortlists xs = sortBy (\xs ys -> compare (length xs) (length ys)) xs

-- Exercise 7
intersperse _ [] = ""
intersperse _ (x:[]) = x
intersperse y (x:xs) = x ++ [y] ++ (intersperse y xs)

-- Exercise 8
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeHeight Empty = 0
treeHeight (Node _ a b) = 1 + (max (treeHeight a) (treeHeight b))

-- Exercise 9
data Direction = LeftTurn
               | RightTurn
               | Straight
               deriving (Show)

-- Exercise 10
findTurn (x1, y1) (x2, y2) (x3, y3)
  | cross == 0 = Straight
  | cross > 0  = LeftTurn
  | cross < 0  = RightTurn
   where cross = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

-- Exercise 11
findTurns (x:y:z:[]) = [findTurn x y z]
findTurns (x:y:z:xs) = (findTurns (x:y:z:[])) ++ (findTurns (y:z:xs))

-- Exercise 12
findP xs = head (sortBy (\(x1,y1) (x2,y2) -> if y1 == y2 then compare x1 x2 else compare y1 y2) xs)

sortPoints (px,py) xs = sortBy (\(x1,y1) (x2,y2) -> compare ((-(x1 - px)) / (y1 - py)) ((-(x2 - px)) / (y2 - py))) xs

checkTurn hull _ (_:_:[])             = hull
checkTurn hull (RightTurn:ts) (x:xs)  = checkTurn hull ts xs
checkTurn hull (LeftTurn:ts) (x:y:xs) = checkTurn (y:hull) ts (y:xs)
checkTurn hull (Straight:ts) (x:xs)   = checkTurn hull ts xs

grahamScan xs =
  checkTurn [p] turns (ps ++ [p])
  where p = findP xs
        ps = p : sortPoints p (delete p xs)
        turns = findTurns (ps ++ [p])
