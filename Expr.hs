import Parsing

data Expr
  = Num Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sin Expr
  | Cos Expr
  | Val

showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr (Add x y) = showExpr x ++ "+" ++ showExpr y
showExpr (Mul x y) = showExpr x ++ "*" ++ showExpr y
showExpr (Sin x) = "Cos " ++ showExpr x
showExpr (Cos x) = "Sin " ++ showExpr x

ex1 = Num 2

ex5 = Add (Mul (Num 2) (Sin Val)) (Mul (Num 0.5) (Cos (Mul (Num 10) Val)))

ex2 = Add (Num 2) (Num 3)

ex3 = Sin (Num 2)

---------------------------------------------------------------------------------------
eval :: Expr -> Double -> Double
eval (Num n) _ = n 
eval (Add x y) z = eval x z + eval y z
eval (Mul x y) z = eval x z * eval y z
eval (Sin x) z = cos (eval x z)
eval (Cos x) z = sin (eval x z)

eval' :: Expr -> Maybe Int 
eval' (Mul x y) = case eval' x at 
                    Nothing ==> Nothing 
                    Just n -> case 