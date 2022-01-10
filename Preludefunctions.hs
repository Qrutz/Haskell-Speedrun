import Data.Char (toUpper)
import Data.List (transpose)

-- all return true if all elements of list satisfy the boolean function
all :: (a -> Bool) -> [a] -> Bool
all p xs = and (map p xs)

-- init returns all but the last element of its argument list . cant be applied to empty list--
init' :: [a] -> [a]
init' [x] = []
init' (x : xs) = x : init' xs

-- iterate returns the infinite list when a func is applied to any
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

-- lines return a list of by breaking the originial list by the newline char.
lines' :: String -> [String]
lines' (x : xs) = l : ls
  where
    (l, xs') = break (== '\n') (x : xs)
    ls
      | null xs' = []
      | otherwise = lines (tail xs')

-- map takes a function and a list and returns the list with the function applied onto it --
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

take' 0 _ = []
take' _ [] = []
take' n (x : xs)
  | n > 0 = x : take' (n - 1) xs
take' _ _ = error "PreludeList.take: negative argument"

-- takewhile takes eleement from front of list when predicate is satisfied
takewhile :: (a -> Bool) -> [a] -> [a]
takewhile p [] = []
takewhile p (x : xs)
  | p x = x : takewhile p xs
  | otherwise = []

-- unlines converts a lsit of strings intoa single string placing a newline character between each of them. its the opposite of the function lines.
unlines' :: [String] -> String
unlines' = concatMap addNewLine
  where
    addNewLine l = l ++ "\n"

-- concat takes a list of lists and returns one list with all the elements of each lists combined
concat' :: [[a]] -> [a]
concat' = foldr (++) []

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_ : xs) | n > 0 = drop' (n - 1) xs
drop' _ _ = error "PreludeList.drop: negative argument"

dropWhile' p [] = []
dropWhile' p (x : xs)
  | p x = dropWhile' p xs
  | otherwise = x : xs

filter' p xs = [k | k <- xs, p k]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p [] = []
filter'' p (x : xs)
  | p x = x : filter'' p xs
  | otherwise = filter'' p xs

counteven = length . filter even

countnegative = length . filter (< 0)

countFalse = length . filter not

iter n f = map f . n

countLength :: Int -> String -> Int
countLength n s =
  let arrOfLengths = map length (words s)
   in length (filter (== n) arrOfLengths)

prop_countLength n s =
  let m = countLength n s
   in m * n <= length s && m <= length (words s)

prop_countLength' n s = length $ words s

countLength' n s = length $ filter (== n) $ map length $ words s

data Suit = Hearts | Clubs | Diamonds | Spades
  deriving (Eq)

data Rank = Numeric Int | Jack | Queen | King | Ace
  deriving (Eq)

data Hand = Empty | Add Card Hand

data Card = Card Rank Suit
  deriving (Eq)

rank (Card r _) = r

ex3h :: Hand
ex3h = Add (Card (Numeric 8) Spades) (Add (Card King Spades) Empty)

ex4h = Add (Card Ace Clubs) (Add (Card Ace Spades) Empty)

moreAcesThan :: Hand -> Hand -> Bool
moreAcesThan h1 h2 = aces h1 > aces h2

aces :: Hand -> Integer
aces Empty = 0
aces (Add c h)
  | rank c == Ace = 1 + aces h
  | otherwise = aces h

rep :: Int -> String -> IO ()
rep n s
  | n <= 0 = return ()
  | otherwise = do
    putStrLn s
    rep (n - 1) s

rep' :: Int -> String -> IO ()
rep' n s = putStr w
  where
    w :: String
    w = unlines $ replicate n s

data Play = O | X -- The letter O not the number 0 (zero)
  deriving (Show, Eq)

data TicTac = TicTac Int [[Maybe Play]]
  deriving (Show)

exampleTT =
  TicTac
    3
    [ [Just O, Just X, Nothing],
      [Just X, Just O, Nothing],
      [Just X, Nothing, Just O]
    ]

printBoard :: TicTac -> IO ()
printBoard b = mapM_ (putStrLn . map converter) (rows b)

converter :: Maybe Play -> Char
converter (Just X) = toUpper 'x'
converter (Just O) = toUpper 'o'
converter Nothing = ' '

rows (TicTac _ b) = b

y = transpose (rows exampleTT)