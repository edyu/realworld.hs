{-# OPTIONS_GHC -Wall #-}

import Data.Char (digitToInt)

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
  where accInt (Right acc) x | x >= '0' && x <= '9' = let intmax = (maxBound :: Int) `div` (digitToInt x)
                                              in  if acc < intmax
                                                  then Right (acc * 10 + (digitToInt x))
                                                  else Left (xs ++ " is too big")
                             | otherwise = Left ("non-digit '" ++ [x] ++ "'")
        accInt (Left err) _ = Left err
