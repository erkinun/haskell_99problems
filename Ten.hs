module Ten 
( myLast
, myButLast
, elementAt
, myLength
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

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = innerLength xs 1

innerLength :: [a] -> Int -> Int
innerLength [] n = n
innerLength (x:xs) n = innerLength xs (n+1)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
