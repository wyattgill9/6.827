data Term
  = Var String         
  | Lam String Term    
  | App Term Term       
  deriving (Show, Eq)

normalOrder :: Term -> Term
normalOrder term = case term of
  Var x -> Var x

  Lam x body -> Lam x (normalOrder body)

  App t1 t2 ->
    case normalOrder t1 of
      Lam x body -> normalOrder (substitute body x t2)
      t1' -> App t1' (normalOrder t2)

substitute :: Term -> String -> Term -> Term
substitute (Var y) x term
  | y == x    = term
  | otherwise = Var y
substitute (Lam y body) x term
  | y == x    = Lam y body
  | otherwise = Lam y (substitute body x term)
substitute (App t1 t2) x term =
  App (substitute t1 x term) (substitute t2 x term)

-- Ex
main :: IO ()
main = do
  let term = App (Lam "y" (Lam "z" (Var "y"))) (App (Lam "x" (App (Var "x") (Var "x"))) (Lam "x" (App (Var "x") (Var "x"))))
  print $ normalOrder term


{-

    Running li on the term (λy.λz.y) ((λx.x x) (λx.x x)) 
    will result in non-termination because the applicative-order interpreter 
    tries to evaluate the non-terminating subterm ((λx.x x) (λx.x x)).

    The applicative-order λ-calculus is not strongly normalizing because it 
    allows terms that do not terminate, such as ((λx.x x) (λx.x x)).

-}