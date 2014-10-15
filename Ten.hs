module Ten 
( myLast
, myButLast
, elementAt
) where

myLast :: [a] -> a
myLast [] = error "can not work on empty list"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "can not work on empty list"
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "can not work on empty list"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)
