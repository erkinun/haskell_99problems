
myLast :: [a] -> a
myLast [] = error "can not work on empty list"
myLast (x:[]) = x
myLast (x:xs) = myLast xs
