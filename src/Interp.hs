module Interp where

import AST (Term (App, Lam, Var))
import Data.List

_freshNames :: ([Name], [Int]) -> [Name]
_freshNames (x : xs, f : rest) =
  (x ++ show f) : _freshNames (xs, rest)
_freshNames ([], rest) = []

freshNames :: [Name] -> [Name]
freshNames used =
  let lst = [1 ..]
   in _freshNames (used, lst)

boundVars :: Term -> [Name]
boundVars (Var n1) = []
boundVars (Lambda n1 e1) = [n1] ++ boundVars e1
boundVars (App e1 e2) = boundVars e1 ++ boundVars e2

freeVars :: Term -> [Name]
freeVars (Var n1) = [n1]
freeVars (Lambda n1 e2) = freeVars e2 \\ [n1]
freeVars (App e1 e2) = freeVars e1 ++ freeVars e2

-- This is used as part of alpha conversion.
renameVar :: (Name, Name) -> Term -> Term
renameVar (old, new) (Var x) =
  if old == x
    then Var new
    else Var x
renameVar (old, new) (Lambda n e) =
  if n == old
    then Lambda n e
    else Lambda n (renameVar (old, new) e)
renameVar (old, new) (App e1 e2) =
  App (renameVar (old, new) e1) (renameVar (old, new) e2)

-- alphaEquivSub fresh (v, e) target
alphaEquivSub :: [Name] -> (Name, Term) -> Term -> ([Name], Term)
alphaEquivSub fresh (v, e) (Var x) =
  if v == x
    then (fresh, e)
    else (fresh, (Var x))
alphaEquivSub fresh (v, e) (Lambda n1 e1) =
  -- Don't propagate.
  if v == n1
    then (fresh, Lambda n1 e1)
    else -- now we need to check for collisions.

      if n1 `elem` freeVars e
        then
          let e1' = renameVar (n1, (head fresh)) e1
           in let (fresh', e2) = alphaEquivSub (tail fresh) (v, e) e1'
               in (fresh', Lambda (head fresh) e2)
        else
          let (fresh', e1') = alphaEquivSub (tail fresh) (v, e) e1
           in (fresh', Lambda n1 e1')
alphaEquivSub fresh (v, e) (App e1 e2) =
  let (fresh1, e1') = alphaEquivSub fresh (v, e) e1
   in let (fresh2, e2') = alphaEquivSub fresh1 (v, e) e2
       in (fresh2, App e1' e2')

normNF_OneStep :: ([Name], Term) -> Maybe ([Name], Term)
normNF_OneStep (fresh, (Lambda n e)) =
  let v = normNF_OneStep (fresh, e)
   in case v of
        Just (fresh', e') -> Just (fresh', Lambda n e')
        Nothing -> Nothing
normNF_OneStep (fresh, (App e1 e2)) =
  case e1 of
    Lambda n e3 ->
      let (fresh', e3') = alphaEquivSub fresh (n, e2) e3
       in Just (fresh', e3')
    _ ->
      -- First, we step into e1.
      let v = normNF_OneStep (fresh, e1)
       in case v of
            Just (fresh', e1') -> Just (fresh', App e1' e2)
            -- If no reduction occurs, we attempt to step into e2.
            Nothing ->
              let k = normNF_OneStep (fresh, e2)
               in case k of
                    Just (fresh', e2') -> Just (fresh', App e1 e2')
                    Nothing -> Nothing
normNF_OneStep (fresh, _) = Nothing

normNF_n :: Int -> ([Name], Term) -> ([Name], Term)
normNF_n c (fresh, e) =
  if c == 0
    then (fresh, e)
    else
      let e' = normNF_OneStep (fresh, e)
       in case e' of
            Nothing -> (fresh, e)
            Just (_, new) ->
              let fresh' = freshNames $ boundVars new
               in normNF_n (c - 1) (fresh', new)

normNF :: Int -> Term -> Term
normNF c e =
  let used = boundVars e
   in let (_, e') = normNF_n c (freshNames used, e)
       in e'
