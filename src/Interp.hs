module Interp where

import AST (Term (App, Lam, Var))

get_names :: Term -> [String]
get_names (Var s) = [s]
get_names (App t1 t2) = get_names t1 ++ get_names t2
get_names _ = []

get_bound :: Term -> [String]
get_bound (Lam s t) = get_names s
get_bound _ = []

get_frees :: Term -> [String]
get_frees (Var s) = [s]
get_frees (Lam v t) =
  let bound = get_bound v
   in [x | x <- get_names t, not (x `elem` bound)]
get_frees _ = []

-- alpha :: Term -> Term
-- alpha _ = _
-- alpha App t1 t2 =
--   let names = get_names t1
--    in App t1 $ rename t2 names
--
-- beta :: Term -> Term
-- beta _ = _
-- beta App t1 t2 =
--   let bound = get_bound t1
--    in let term = get_term t1
--        in replace term bound t2
--
-- step :: Term -> Term
-- step = beta $ alpha
