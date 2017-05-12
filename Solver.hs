{-# LANGUAGE GADTs #-}

{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Implements an evaluator, satisfiability checker and solver for logical Formulas.
-}

module Solver where

import Formula
import Data.List

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
satisfiable (Body t)     = eval t -- unquantified Formula is satisfiable iff it evaluates to True
satisfiable (Exists v f) = checkAllInstantiations v f -- where v is the set of values & f is the Formula
  where
    -- tries all values of the quantified variables
    checkAllInstantiations :: [a] -> (Term a -> Formula as) -> Bool
    checkAllInstantiations v f =
      if length v == 0
      then False -- run out of instantiations
      else case (f (Con (v !! 0))) of
             (Exists v' f') -> satisfiable $ Exists v' f' -- Formula may have multiple quantified variables
             (Body term)    -> eval term -- otherwise, return what the Term evaluates to
           || checkAllInstantiations (tail v) f

-- computes a list of all the solutions of a Formula
solutions :: Formula ts -> [ts]
solutions (Body t)        = [()] -- solution to an unquantified Forumula is trivial
solutions f'@(Exists v f) = getSols f' -- nubBy cmp $ getSols f'
--solutions (Exists v f) = findSol v f []
--solutions (Exists v f) = [(head v, solutions (f (Con (head v)))), (solutions (Exists (tail v) f))]
  where
    -- compares two nested tuples (in order to remove duplicates using num)
--    cmp :: ts1 -> ts2 -> Bool
--    cmp () () = True
--    cmp a b = fst a == fst b -- && cmp (snd a) (snd b)

    -- finds all possible sols
    getSols :: Formula ts -> [ts]
    getSols (Body t)     = [()] -- trivial case
    getSols (Exists v f) =
      if length v == 0
      then [] -- run out of instantiations
      else let
             (sol, isSol) = getSol (Exists [head v] f)
             rest         = case (f (Con (head v))) of
                                   (Body t)       -> []
                                   (Exists v' f') -> putHeadInFront (head v) (getSols (Exists v' f'))
           in if isSol == True
              then sol : -- sol using 1st value of 1st quantified variable
                   ((getSols (Exists (tail v) f)) ++ -- sol using rest of values for 1st quantified variable
                   rest) -- sol using all values of rest of quantified variables
              else ((getSols (Exists (tail v) f)) ++ -- sol using rest of values for 1st quantified variable
                   rest) -- sol using all values of rest of quantified variables

    -- returns sol by expanding the lambda terms of the Formula
    -- also returns what the resulting term evaluates to
    getSol :: Formula ts -> (ts, Bool)
    getSol (Body t)     = ((), eval t)
    getSol (Exists v f) = (((head v), sol), isSol) -- sol is a nested tuple
      where
        f'           = f (Con (head v)) -- expand rest of terms
        (sol, isSol) = getSol f'

    -- re-inserts the head value into the sol to create a valid list of tuples of type (a,as) not as
    putHeadInFront :: a -> [(b,c)] -> [(a,(b,c))]
    putHeadInFront h []     = []
    putHeadInFront h (x:xs) = (h, x) : putHeadInFront h xs

{-
-------------------------------
    -- tries all values of the quantified variables
    findSol :: [a] -> (Term a -> Formula as) -> [ts] -> [ts]
    findSol v f m =
      if length v == 0
      then m
      else case (f (Con (head v))) of
             (Exists v' f') -> let
--                                 a = solutions $ Exists v' f'
                                 a = findSol v' f' $ putHeadInFront (head v) m
                                 b = a --putHeadInFront (head v) a
                               in b
             (Body term)    -> putHeadInFront (head v) m
           ++ findSol (tail v) f m

    putHeadInFront :: a -> [(b,c)] -> [(a,(b,c))]
    putHeadInFront h [] = [h]
    putHeadInFront h (x:xs) = (h, x) : putHeadInFront h xs
---------------------------------
-}
