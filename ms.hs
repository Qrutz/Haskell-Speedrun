import Data.Maybe (listToMaybe)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []aa
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

-- if the key matches key then return (k,v)

lookupAll :: Eq key => key -> [(key, value)] -> [value]
lookupAll key kvs = [v | (k, v) <- kvs, k == key]

-- make a defintion of the standard lookup function --
lookup' :: Eq key => key -> [(key, a)] -> Maybe a
lookup' key = listToMaybe . lookupAll key

-- if key is in (k,v) then replace v with val, else append the pair

update :: Eq key => key -> value -> [(key, value)] -> [(key, value)]
update k v [] = [(k, v)]
update k v ((oldk, oldv) : kvs)
  | oldk == k = (oldk, v) : kvs
  | otherwise = (oldk, oldv) : update k v kvs

data Expr = X | Num Int | Op BinOp Expr Expr
  deriving (Eq)

data BinOp = Add | Mul | Subtract
  deriving (Eq, Show)

eval :: Expr -> Int -> Int
eval (Num n) _ = n
eval X x = x
eval (Op Add a b) x = eval a x + eval b x
eval (Op Subtract a b) x = eval a x - eval b x
eval (Op Mul a b) x = eval a x * eval b x

removeSub_v2 :: Expr -> Expr
removeSub_v2 (Op Add e1 e2) = Op Add (removeSub_v2 e1) (removeSub_v2 e2)
removeSub_v2 (Op Mul e1 e2) = Op Mul (removeSub_v2 e1) (removeSub_v2 e2)
removeSub_v2 (Op Subtract e1 e2) = Op Add r1 (Op Mul (Num (-1)) r2)
  where
    r1 = removeSub_v2 e1
    r2 = removeSub_v2 e2
removeSub_v2 e = e

ex1 = Op Subtract (Num 100) X

ex2 = Op Add (Num 100) (Op Mul (Num (-1)) X)

showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr (Op Add a b) = showExpr a ++ "+" ++ showExpr b
showExpr (Op Subtract a b) = showExpr a ++ "-" ++ showExpr b
showExpr (Op Mul a b) = showExpr a ++ "*" ++ showExpr b
showExpr X = "x"

instance Show Expr where
  show = showExpr

data Grid a = Grid [[a]]
  deriving (Eq, Show)

g1, g2 :: Grid Int -- Example grids
g1 =
  Grid
    [ [1, 2],
      [3, 4],
      [5, 6]
    ]
g2 =
  Grid
    [ [5, 3, 1],
      [6, 4, 2]
    ]

mapGrid :: (a -> b) -> Grid a -> Grid b
mapGrid f = Grid . map (map f) . rows
