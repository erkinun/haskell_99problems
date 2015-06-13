module Ten 
( myLast
, myButLast
, elementAt
, myLength
, myReverse
, isPalindrome
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

--isPalindrome :: [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs | (head xs) == (last xs) = True && isPalindrome( init $ tail xs )
                | otherwise = False 

data NestedList a = Elem a | List [NestedList a]

flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (a:as)) = (flatten a) ++ flatten (List as)

compress [] = [] 
compress (a:b:xs)
    | a == b = compress (b:xs)
    | otherwise = a : compress (b:xs)                       
compress (x:xs) = x : compress xs

pack :: Eq a => [a] -> [[a]]
pack [] = [] 
pack xs = packInner xs []

packInner :: Eq a => [a] -> [[a]] -> [[a]]
packInner (x:xs) [] = packInner xs [[x]]
packInner (x:xs) xss
          | elem x (last xss) = packInner xs $  (init xss) ++ [(x : (last xss))]
          | otherwise = packInner xs $ xss ++ [[x]]
packInner [] xss = xss 

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = (encodeInner . pack) xs

encodeInner :: [[a]] -> [(Int,a)]
encodeInner [] = []
encodeInner (x:xs) = ((length x), (head x)) : encodeInner xs

data RunLength a = Multiple Int a | Single a deriving (Show)

encodeModified [] = []
encodeModified xs = (encodeModInner.encode) xs

encodeModInner :: Eq a => [(Int, a)] -> [RunLength a]
encodeModInner [] = []
encodeModInner (x:xs)
    | fst x == 1 = Single (snd x) : encodeModInner xs
    | otherwise = Multiple (fst x) (snd x) : encodeModInner xs

decodeModified :: [RunLength a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = (run2List x) ++ decodeModified xs

run2List :: RunLength a -> [a]
run2List (Single a) = [a]
run2List (Multiple len a) = replicate len a
