
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Expr where
import Parsing
import qualified Data.Functor
import Test.QuickCheck
import Data.Attoparsec.ByteString.Char8 (isSpace)
import Data.Maybe (fromJust)



-- 1. A.                                                                                                                                                            -- 
data Expr = Num Double
        | Add Expr Expr
        | Mul Expr Expr
        | Sin Expr
        | Cos Expr
        | Var
        deriving(Eq)


x :: Expr
x = Var

num :: Double -> Expr
num = Num

add,mul :: Expr -> Expr -> Expr
add = Add
mul = Mul

size :: Expr -> Integer
size (Num n) = 1
size (Add e1 e2) = 1 + size e1 + size e2
size (Mul e1 e2) = 1 + size e1 + size e2
size (Sin e) = 1
size (Cos e) = 1
size Var = 1

--B-- 
showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr (Add a b) = showExpr a ++ "+" ++ showExpr b
showExpr (Mul a b) = showFactor a ++ "*" ++ showFactor b
showExpr (Sin a) = "sin" ++ showFactor a ++ " "
showExpr (Cos a) = "cos" ++ showFactor a ++ " "
showExpr Var  = "x"


showFactor :: Expr -> String
showFactor (Num n) = showExpr (Num n)
showFactor (Add a b) = "(" ++ showExpr (Add a b) ++ ")"
showFactor (Sin a) = showExpr (Sin a)
showFactor (Cos a) = showExpr (Cos a)
showFactor Var = showExpr Var
showFactor e = showExpr e


instance Show Expr where
        show = showExpr

-- C -- 

eval :: Expr -> Double -> Double
eval (Num n) _ = n
eval Var x = x
eval (Add a b) x = eval a x + eval b x
eval (Mul a b) x = eval a x + eval b x
eval (Sin a) x = sin (eval a x)
eval (Cos a) x = cos (eval a x)

-- D -- 

readExpr :: String -> Maybe Expr
readExpr s = case parse expr (rmWhiteSpace s) of
        Just (e,"") -> Just e
        _ -> Nothing

rmWhiteSpace :: String -> String
rmWhiteSpace = filter(not . isSpace)

expr, term, factor :: Parser Expr

expr = leftAssoc Add term (char '+')

term = leftAssoc Mul factor (char '*')


factor = doubleP <|> paranthesisP <|> variableP <|> sinP <|> cosP


doubleP :: Parser Expr
doubleP = Num <$> readsP

paranthesisP :: Parser Expr
paranthesisP = char '(' *> expr <* char ')'

variableP:: Parser Expr
variableP = char 'x' Data.Functor.$> Var

sinP :: Parser Expr
sinP = do
        char 's'
        char 'i'
        char 'n'
        Sin <$> factor

cosP :: Parser Expr
cosP = do
        char 'c'
        char 'o'
        char 's'
        Cos <$> factor


leftAssoc :: (t->t->t) -> Parser t -> Parser sep -> Parser t
leftAssoc op item sep = foldl1 op <$> chain item sep

----------------------------- D -----------------------


prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = showExpr e == showExpr (fromJust $ readExpr $ showExpr e)

instance Arbitrary Expr where
    arbitrary = sized arbExpr

arbExpr :: Int -> Gen Expr
arbExpr s =  frequency [ (1, Num <$> arbitrary)
                       , (s, do a <- arbExpr s'
                                b <- arbExpr s'
                                return (Add a b))
                       , (s, do a <- arbExpr s'
                                b <- arbExpr s'
                                return (Mul a b)) ]
    where s' = s `div` 2

---------------------------E ---------------------------
simplify :: Expr -> Expr
simplify (Num n) = Num n
simplify (Add a b) = addHelper (simplify a) (simplify b)
simplify (Mul a b) = mulHelper (simplify a) (simplify b)
simplify (Sin a) = sinHelper (Sin a)
simplify (Cos a) = cosHelper (Cos a)
simplify Var = Var

addHelper :: Expr -> Expr -> Expr
addHelper a (Num 0) = simplify a
addHelper (Num 0) b = simplify b
addHelper a b = add (simplify a) (simplify b)

mulHelper :: Expr -> Expr -> Expr
mulHelper a (Num 0) = Num 0
mulHelper (Num 0) b = Num 0
mulHelper a (Num 1) = a
mulHelper (Num 1) b = b
mulHelper (Num(-1)) (Num b) = Num (-b)
mulHelper (Num a) (Num(-1)) = Num (-a)
mulHelper a b = mul (simplify a) (simplify b)



sinHelper :: Expr -> Expr

sinHelper (Sin (Num n)) | n == 0 || n == pi = Num 0
                        | n == pi/2         = Num 1
sinHelper (Sin n)                           = Sin (simplify n)

cosHelper :: Expr -> Expr
cosHelper (Cos (Num n))
                  | n == 0 || n == pi = Num 1
                  | n == pi/2         = Num 0
cosHelper (Cos n)                     = Cos (simplify n)



prop_simplify :: Expr -> Bool
prop_simplify e = eval e 0 == eval (simplify e) 0



--------- G ---------

differentiate :: Expr -> Expr
differentiate (Add e1 e2) = simplify $ add (differentiate e1) (differentiate e2)
differentiate (Mul e1 e2) = simplify $ add (mul (differentiate e1) e2) (mul e1 (differentiate e2))
differentiate Var         = Num 1
differentiate (Mul Var Var)     = mul (Num 2) Var
differentiate (Cos e)     = simplify $ mul (differentiate e) (Cos e)
differentiate (Sin e)     = simplify $ mul (Num (-1)) (simplify $ mul (differentiate e) (Sin e))
differentiate (Num _)         = Num 0



exS1 = simplify (Add Var (Add (Num 2) Var))

exS1' = simplify (Add Var (Mul (Num 0) Var))
ex2'' = Sin (Num 2)
ex1' = Add (Num 1) (Mul (Num 2) (Num 3))
ex2' = "2*3+(4*5)"
ex3' = "sin(x)"
ex4' = "sin (cos x)"
ex5' = "(sin x) + (cos x)"
ex6' = "sin cos x"
ex7' = "sin (-7.321271617241244+sin 1.3841251597435147 +cos 21.38739735015702)"
