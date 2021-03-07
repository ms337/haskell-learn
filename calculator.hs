data ExprT = Lit Integer
            | Add ExprT ExprT
            | Mul ExprT ExprT
        deriving (Show, Eq)

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add exp1 exp2) = (eval exp1) + (eval exp2)
eval (Mul exp1 exp2) = (eval exp1) * (eval exp2)