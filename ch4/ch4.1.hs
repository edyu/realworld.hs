{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (_:xs) = Just (last xs)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

-- Exercise 2
-- splitWith odd [1,3,4,5,6, 7, 9] = [[1,3], [5], [7, 9]]
--splitWith (a-> Bool) -> ([a], [a])
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = let (hs, ts) = break (not . f) xs
                 in  case (hs, ts) of
                       ([], [])   -> []
                       (h, [])    -> [h]
                       ([], _:tt) -> splitWith f tt
                       (h, _:tt)  -> h : (splitWith f tt)
