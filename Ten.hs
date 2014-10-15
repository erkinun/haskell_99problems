module Ten 
( myLast,
  myButLast
) where

myLast :: [a] -> a
myLast [] = error "can not work on empty list"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "can not work on empty list"
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs
