-- 1
myLast :: [a] -> a
myLast []     = undefined
myLast [x]    = x
myLast (_:xs) = myLast xs

--2
myButLast :: [a] -> a
myButLast []     = undefined
myButLast [x]    = undefined
myButLast [x,y]  = x
myButLast (_:xs) = myButLast xs

--3
elementAt :: [a] -> Integer -> a
elementAt [] _     = undefined
elementAt (x:xs) 0 = undefined
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n - 1)

--4
myLength :: [a] -> Integer 
myLength []     = 0
myLength (x:xs) = 1 + myLength xs

--5
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

--6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome l  = (l == myReverse l)

--7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = flattenList x

flattenList :: [NestedList a] -> [a]
flattenList []     = []
flattenList (x:xs) = flatten x ++ flattenList xs

--8
compress :: Eq a => [a] -> [a]
compress []  = []
compress [x] = [x]
compress (x:(y:ys))
    | x == y    = compress (y:ys)
    | otherwise = x : compress (y:ys)

--9
pack :: Eq a => [a] -> [[a]]
pack []  = []
pack [x] = [[x]]
pack (x:(y:ys)) 
    | x == y    = [[x,y]]  ++ pack ys
    | otherwise = pack [x] ++ pack (y:ys)

myFirst :: [a] -> a
myFirst []     = undefined 
myFirst (x:xs) = x











