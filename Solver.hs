{-# LANGUAGE GADTs #-}

{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Implements an evaluator, satisfiability checker and solver for logical Formulas.
-}

module Solver where

import Formula

-- evaluates a logical Term
eval :: Term t -> t
eval (Con         n) = n
eval (And     t1 t2) = eval t1 && eval t2
eval (Or      t1 t2) = eval t1 || eval t2
eval (Smaller n1 n2) = eval n1 < eval n2
eval (Plus    n1 n2) = eval n1 + eval n2
eval (Name _)        = error "eval: Name" -- not relevant for evaluation

-- determines whether a Formula is satisfiable
satisfiable :: Formula ts -> Bool
satisfiable _ = error "FIXME: implement satisfiable"

-- computes a list of all the solutions of a Formula
solutions :: Formula ts -> [ts]
solutions _ = error "FIXME: implement solutions"
