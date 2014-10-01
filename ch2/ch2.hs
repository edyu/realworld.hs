-- Exercise 2
lastButOne :: [a] -> a
lastButOne (x:y:[]) = x
lastButOne (x:y:xs) = lastButOne (y:xs)
