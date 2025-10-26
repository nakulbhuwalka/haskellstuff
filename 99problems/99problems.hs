-- from https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

myLast :: [a] -> a
-- myLast = foldl1 (\_ x -> x)
myLast [x] = x
myLast (_:xs) = myLast xs


myButLast :: [a] -> a
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "List too short"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)

myLength :: [a] -> Int
myLength = foldl (\acc _ -> acc+1) 0

myReverse :: [a] ->[a]
myReverse = myReverseInternal []
    where
    	myReverseInternal result [] = result
    	myReverseInternal result (x:xs) = myReverseInternal (x:result) xs

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == (myReverse x)

-- TODO flatten
compress :: (Eq a) => [a] -> [a]
compress (x:xs) = compressInternal x xs
    where
    	compressInternal last [] = [last]
    	compressInternal last (x:xs) = if last /=x then last:compressInternal x xs else compressInternal x xs

pack :: (Eq a) => [a] -> [[a]]
pack (x:xs) = packInternal [x] xs
    where 
    	packInternal internal [] = [internal] 
    	packInternal internal (x:xs) = if (head internal) == x then packInternal (x:internal) xs else internal : (packInternal [x] xs) 

encode :: (Eq a) => [a] -> [(Int,a)]
encode (x:xs) = encodeInternal (1,x) xs
    where 
    	encodeInternal internal [] = [internal] 
    	encodeInternal (count,a) (x:xs) = if (a) == x then encodeInternal (count+1,a) xs else (count,a)  : (encodeInternal (1,x) xs)  