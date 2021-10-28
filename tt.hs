-- 1. Last element of a list--
myLast :: [a] -> a
myLast [] = error "list is empty therefore no last element"
myLast [x] = x --if list only has one element--
myLast (_ : xs) = myLast xs

-- 2. Last but one element of a list--

myButLast :: [a] -> a
myButLast [] = error "empty"
myButLast [x] = error "only one element which is "
myButLast [x, _] = x
myButLast (_ : xs) = myButLast xs

-- 3. Find the K'th element of a list. The first element in the list is number 1 --
elementAt :: [a] -> Int -> a
elementAt xs a = xs !! (a - 1)

--4. Find the number of elements of a list. --
myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

-- 5. Reverse a list. --
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- 6. Is Palindrome? --
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

list = [1, 2, 3, 4, 5]
