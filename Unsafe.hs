{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Defines an evaluator for untyped terms to construct logical formulas.
-}

module Unsafe where

-- untyped terms of a formula
data Term
  = Con     Val
  | And     Term Term
  | Or      Term Term
  | Smaller Term Term
  | Plus    Term Term

-- possible result types of evaluating the untyped terms
data Val
  = IntV  Int
  | BoolV Bool
  deriving (Show)

-- evaluation of untyped terms including type checking
eval :: Term -> Val
eval (Con v)   = v
eval (And p q) =
  case (eval p, eval q) of
    (BoolV b1, BoolV b2) -> BoolV $ b1 && b2
    _                    -> error "And: expected Bool arguments"
eval (Or p q) =
  case (eval p, eval q) of
    (BoolV b1, BoolV b2) -> BoolV $ b1 || b2
    _                    -> error "Or: expected Bool arguments"
eval (Smaller n m) =
  case (eval n, eval m) of
    (IntV i1, IntV i2) -> BoolV $ i1 < i2
    _                  -> error "Smaller: expected Int arguments"
eval (Plus n m) =
  case (eval n, eval m) of
    (IntV i1, IntV i2) -> IntV $ i1 + i2
    _                  -> error "Plus: expected Int arguments"

-- some example terms
t1 :: Term
t1 = Con (BoolV True)
t2 :: Term -> Term
t2 n = n `Smaller` (n `Plus` Con (IntV 1))
t3 :: Term -> Term -> Term
t3 p n = p `Or` (Con (IntV 0) `Smaller` n)
