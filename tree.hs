import Data.List

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show)

treeDepth :: Tree Int -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) = 1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

singleTon :: a -> Tree a
singleTon x = Node x Leaf Leaf

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Leaf = singleTon x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

exp2 :: Tree Int
exp2 = Node 2 t1 (Node 1 t1 t0)
  where
    t1 = Node 1 Leaf Leaf
    t0 = Node 0 Leaf Leaf

y = treeInsert 3 exp2

arr :: [Int]
arr = [1, 2, 2, 3, 4]

-- add to back
add :: a -> [a] -> [a]
add x (y : ys) = y : ys ++ [x]

-- add to front --

add' :: a -> [a] -> [a]
add' x (y : ys) = x : ys

lengthArr :: [a] -> Int
lengthArr (x : xs) = 1 + lengthArr xs

remove :: [Int] -> [Int]
remove (x : xs) = xs

alreadyVisited :: Int -> [Int] -> Bool
alreadyVisited x [] = False
alreadyVisited x (v : visisted)
  | x == v = True
  | otherwise = alreadyVisited x visisted

lol arr = length arr /= length (nub arr)