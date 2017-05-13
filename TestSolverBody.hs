#!/usr/bin/runhaskell

{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Tests the solver for unquantified Formulas.
-}

module TestSolverBody where

import Formula
import Solver

main :: IO ()
main = do

  -- constant Formulas
  assert (Body (Con True)) [()]
  assert (Body (Con False)) [()]

  -- "And" Formulas
  assert (Body (And (Con True) (Con True))) [()]
  assert (Body (And (Con True) (Con False))) [()]
  assert (Body (And (Con False) (Con True))) [()]
  assert (Body (And (Con False) (Con False))) [()]

  -- "Or" Formulas
  assert (Body (Or (Con True) (Con True))) [()]
  assert (Body (Or (Con True) (Con False))) [()]
  assert (Body (Or (Con False) (Con True))) [()]
  assert (Body (Or (Con False) (Con False))) [()]

  -- "Smaller" Formulas
  assert (Body (Smaller (Con 0) (Con 1))) [()]
  assert (Body (Smaller (Con 5) (Con 10))) [()]
  assert (Body (Smaller (Con (-4)) (Con 0))) [()]
  assert (Body (Smaller (Con (-8)) (Con (-6)))) [()]
  assert (Body (Smaller (Con 12) (Con 1))) [()]
  assert (Body (Smaller (Con 345) (Con (-5)))) [()]
  assert (Body (Smaller (Con 83) (Con (-40)))) [()]
  assert (Body (Smaller (Con 0) (Con 0))) [()]

  -- compound Formulas
  assert (Body (And (And (Con True) (Con True)) (Con False))) [()]
  assert (Body (Or (Or (Con True) (Con True)) (Con True))) [()]
  assert (Body (Or (And (Con False) (Con False)) (Con False))) [()]
  assert (Body (And (Con True) (And (Con True) (Con False)))) [()]
  assert (Body (Or (Or (Con True) (Con True)) (Con False))) [()]
  assert (Body (And (And (Con True) (Con True)) (Con True))) [()]
  assert (Body (And (Con True) (Or (Con False) (Con False)))) [()]
  assert (Body (Or (Or (Con True) (Con False)) (Con True))) [()]
  assert (Body (Or (Smaller (Con 1) (Con 1)) (Con True))) [()]
  assert (Body (And (Con True) (Smaller (Con 4) (Con 6)))) [()]
  assert (Body (Smaller (Plus (Con 4) (Con 12)) (Con 19))) [()]
  assert (Body (Or (Smaller (Con 4) (Con 2)) (Con True))) [()]

-- prints True if the solver returns the right solution set; False otherwise
assert :: Eq a => Formula a -> [a] -> IO ()
assert formula result = putStrLn $ show $ solutions formula == result
