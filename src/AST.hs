module AST (Term (App, Lam, Var)) where

data Term
  = Var String
  | Lam Term Term
  | App Term Term

showTerm :: Term -> String
showTerm (Var s) = s
showTerm (Lam t1 t2) = "ƛ(" ++ showTerm t1 ++ ")." ++ showTerm t2
showTerm (App t1 t2) = "(" ++ showTerm t1 ++ " " ++ showTerm t2 ++ ")"

instance Show Term where
  show = showTerm
