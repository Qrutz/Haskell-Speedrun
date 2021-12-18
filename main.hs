-- QuickSort --
qq :: Ord a => [a] -> [a] 
qq [] = []
qq (x:xs) = let smallerSorted = qq [a | a <- xs, a <= x]
                biggerSorted = qq [a | a <- xs, a > x]
            in smallerSorted ++ [x]  ++ biggerSorted


-- Reverse --
reverse' :: [a] -> [a] 
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- max --
max' :: (Ord a) => [a] -> a 
max' [] = error "0"
max' [x] = x 
max' (x:xs) 
    | x > maxTail = x 
    | otherwise = maxTail 
    where maxTail = max' xs   

max'' :: [Int] -> Int 
max'' [] = error "0"
max'' [x] = x 
max'' (x:xs) = max x (max'' xs)


-- replicate --
repl' :: (Num i, Ord i) => i -> a -> [a] 
repl' n x  
    | n <= 0 = []
    | otherwise = x:repl' (n-1) x

--take--
take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  

-- repeat--
repeat' :: a -> [a]  
repeat' x = x:repeat' x  

--zip--
zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys 

--elem--
elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs 