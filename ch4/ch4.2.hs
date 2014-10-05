{-# OPTIONS_GHC -Wall #-}

import Data.Char (digitToInt, isSpace)

-- Exercise 1
asInt :: String -> Int
asInt xs = foldl accInt 0 xs
  where accInt acc x = acc * 10 + (digitToInt x)

-- Exercise 2
asInt_fold :: String -> Int
asInt_fold ('-':xs) = negate (asInt xs)
asInt_fold [] = 0
asInt_fold xs = asInt xs

-- Exercise 3
asInt_err :: String -> Int
asInt_err ('-':xs) = negate (asInt_err xs)
asInt_err [] = error "not a number"
asInt_err xs = foldl accInt 0 xs
  where accInt acc x | x >= '0' && x <= '9' = let intmax = (maxBound :: Int) `div` (digitToInt x)
                                              in  if acc < intmax
                                                  then acc * 10 + (digitToInt x)
                                                  else error (xs ++ " is too big")
                     | otherwise = error ("non-digit '" ++ [x] ++ "'")

-- Exercise 4
type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either ('-':xs) = case (asInt_either xs) of
                          Left err -> Left err
                          Right n -> Right (negate n)
asInt_either [] = Left "not a number"
asInt_either xs = foldl accInt (Right 0) xs
  where accInt (Right acc) x | x >= '0' && x <= '9' =
          let intmax = (maxBound :: Int) `div` (digitToInt x)
          in  if acc < intmax
              then Right (acc * 10 + (digitToInt x))
              else Left (xs ++ " is too big")
                             | otherwise = Left ("non-digit '" ++ [x] ++ "'")
        accInt (Left err) _ = Left err

-- Exercise 5 & 6
myConcat :: [[a]] -> [a]
myConcat xxs = foldr (++) [] xxs

-- Exercise 7
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x:xs) | p x       = x : myTakeWhile p xs
                     | otherwise = []

myTakeWhiler :: (a -> Bool) -> [a] -> [a]
myTakeWhiler p xs = foldr f [] xs
  where f x acc | p x = x : acc
                | otherwise = []

-- Exercise 8 & 9
myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy p xs = foldr g [] xs
  where g x [] = [[x]]
        g x acc@(y:ys) | p x (head y) = (x : y) : ys
                       | otherwise    = [x] : acc

-- Exercise 10
myAny :: (a -> Bool) -> [a] -> Bool
myAny p xs = foldr f False xs
  where f x acc | p x = True
                | otherwise = acc

myCycle :: [a] -> [a]
myCycle xs = foldr f xs xs
  where f _ acc = acc ++ (myCycle acc)

myWords :: String -> [String]
myWords xs = let zs = foldr f [] xs
             in  case zs of
                   [] -> []
                   (s:sz) | s == "" -> sz
                          | otherwise -> zs
  where f x [] | isSpace x = []
               | otherwise = [[x]]
        f x ([]:ys) | isSpace x = []:ys
                    | otherwise = [x]:ys
        f x (y:ys) | isSpace x = []:(y:ys)
                   | otherwise = (x:y):ys

--myUnlines
myUnlines :: [String] -> String
myUnlines xs = foldr join "" xs
  where join l acc = l ++ "\n" ++ acc
