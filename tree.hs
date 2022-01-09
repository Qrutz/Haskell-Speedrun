import Data.List
import Data.Text (toUpper)

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show)

treeDepth :: Tree Int -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) = 1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

singleTon :: a -> Tree a
singleTon x = Node x Leaf Leafaaa

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

data Sudoku = Sudoku [[Int]]

-- i --
showSudoku (Sudoku s) = unlines $ intersperse hr $ map showRow s
  where
    hr = replicate (9 * 2 - 1) '-'
    showRow = intersperse '|' . map showNum
    showNum 0 = ' '
    showNum n = head (show n)

ex =
  Sudoku
    [ [3, 6, 0, 0, 7, 1, 2, 0, 0],
      [0, 5, 0, 0, 0, 0, 1, 8, 0],
      [0, 0, 9, 2, 0, 4, 7, 0, 0],
      [0, 0, 0, 0, 1, 3, 0, 2, 8],
      [4, 0, 0, 5, 0, 2, 0, 0, 9],
      [2, 7, 0, 4, 6, 0, 0, 0, 0],
      [0, 0, 5, 3, 0, 8, 9, 0, 0],
      [0, 8, 3, 0, 0, 0, 0, 6, 0],
      [0, 0, 7, 6, 9, 0, 0, 4, 3]
    ]

toJaden :: String -> String
toJaden = undefined

n = "How can mirrors be real if our eyes aren't real"

getEachWord = words

add1 :: Num a => a -> a -> a
add1 x y = x + y

mul x y = x * y

ex1 = add1 2 $ mul 2 3

ex2 = add1 2 $ mul 2 $ mul 3 $ add1 1 2

-- (f . g) (a) f(g(a))
times2 :: [Int] -> [Int]
times2 = filter (< 17) . filter (> 12) . map (+ 2) . map (* 2)

duplis :: Eq a => [a] -> [a]
duplis = map head . group

duplisa :: Eq a => [a] -> [a]
duplisa arr = map head $ group arr

regg :: [a] -> [a]
regg (d : x : y : rest) = rest

ex1' :: String -> String
ex1' s = reverse (take 10 (drop 1 (reverse s)))

rump :: String -> String
rump s = reverse $ take 10 $ drop 1 $ reverse s

bading s =
  let x = reverse
      y = take 10
      z = drop 1
      e = reverse s
   in x $ y $ z $ e

myLast :: [Int] -> Int
myLast [] = error "r"
myLast arr = arr !! (length arr - 1)

myButLast :: [Int] -> Int
myButLast [] = error "r"
myButLast arr = arr !! (length arr - 2)

elementAt arr index = arr !! index

myLength [] = 0
myLength (x : xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x : xs) = myReverse xs ++ [x]

isPalindrome arr = arr == myReverse arr

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem n) = [n]
flatten (List []) = []
flatten (List (x : xs)) = flatten x ++ flatten (List xs)

compress s = map head $ group s

pack arr = group arr

encode :: Eq a => [a] -> [(Int, a)]
encode s = zip (map length $ pack s) (map head $ pack s)

data ListItem a = Single a | Multiple Int a
  deriving (Show)

encodeModified s = map encodeHelper $ encode s
  where
    encodeHelper (1, x) = Single x
    encodeHelper (n, x) = Multiple n x

decodeModified s = whitespace $ unwords $ map decodeHelper $ s
  where
    decodeHelper (Multiple n x) = replicate n x
    decodeHelper (Single x) = replicate 1 x

w = decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']

whitespace :: String -> String
whitespace s = [x | x <- s, x /= ' ']

dupli [] = []
dupli (x : xs) = x : x : dupli xs

repli xs n = concatMap (replicate n) xs

dropEvery xs s
  | length xs < s = xs
  | otherwise = take (s - 1) xs ++ dropEvery (drop s xs) s

insertAt :: a -> [a] -> Int -> [a]
insertAt x ys 1 = x : ys
insertAt x (y : ys) n = y : insertAt x ys (n - 1)

-- l--aaaa
aaaaa
aaaaaa
