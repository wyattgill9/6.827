data Expr = Var String
          | Lam String Expr
          | App Expr Expr
          deriving (Show, Eq)

-- Sub: replace all occurrences of a variable with an expression
subst :: String -> Expr -> Expr -> Expr
subst x e (Var y)      = if x == y then e else Var y
subst x e (Lam y body) = if x == y then Lam y body else Lam y (subst x e body)
subst x e (App e1 e2)  = App (subst x e e1) (subst x e e2)

-- Call-by-value interpreter (Like from notes)
eval :: Expr -> Expr
eval (Var x) = Var x
eval (Lam x e) = Lam x e
eval (App e1 e2) = 
    let f = eval e1 
        a = eval e2
    in case f of
        Lam x body -> eval (subst x a body)
        _          -> App f a

-- Ex:
idFunc :: Expr
idFunc = Lam "x" (Var "x")

testExpr :: Expr
testExpr = App idFunc (Var "y") 

main :: IO ()
main = print $ eval testExpr
