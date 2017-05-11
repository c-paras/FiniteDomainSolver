{-# LANGUAGE GADTs #-}

{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Implements an evaluator, satisfiability checker and solver for logical Formulas.
-}

module Solver where

import Formula

-- evaluates a logical Term
eval :: Term t -> t
eval (Con         n) = n -- integer or boolean result
eval (And     t1 t2) = eval t1 && eval t2
eval (Or      t1 t2) = eval t1 || eval t2
eval (Smaller n1 n2) = eval n1 < eval n2
eval (Plus    n1 n2) = eval n1 + eval n2  -- integer result
eval (Name _)        = error "eval: Name" -- not relevant for evaluation

-- determines whether a Formula is satisfiable
satisfiable :: Formula ts -> Bool
satisfiable (Body t)     = eval t -- unquantified formula is satisfiable iff it evaluates to True
satisfiable (Exists v f) = checkAllInstantiations v f -- where v is the set of values & f is the formula
  where
    -- tries all values of the quantified variable
    checkAllInstantiations :: [a] -> (Term a -> Formula as) -> Bool
    checkAllInstantiations v f =
      if length v == 0
      then False
      else checkAllInstantiations (tail v) f ||
           case (f (Con (v !! 0))) of
             (Exists v' f') -> satisfiable $ Exists v' f' -- formula may have multiple quantified variables
             (Body term)    -> eval term -- otherwise, return what the term evaluates to

-- computes a list of all the solutions of a Formula
solutions :: Formula ts -> [ts]
solutions _ = error "FIXME: implement solutions"
