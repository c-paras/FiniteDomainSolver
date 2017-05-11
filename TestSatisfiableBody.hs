#!/usr/bin/runhaskell

{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Tests the satisfiability checking of unquantified Formulas.
-}

module TestSatisfiableBody where

import Formula
import Solver

main :: IO ()
main = do

  -- constant Formulas
  assert (Body (Con True)) True
  assert (Body (Con False)) False

  -- "And" Formulas
  assert (Body (And (Con True) (Con True))) True
  assert (Body (And (Con True) (Con False))) False
  assert (Body (And (Con False) (Con True))) False
  assert (Body (And (Con False) (Con False))) False

  -- "Or" Formulas
  assert (Body (Or (Con True) (Con True))) True
  assert (Body (Or (Con True) (Con False))) True
  assert (Body (Or (Con False) (Con True))) True
  assert (Body (Or (Con False) (Con False))) False

  -- "Smaller" Formulas
  assert (Body (Smaller (Con 0) (Con 1))) True
  assert (Body (Smaller (Con 5) (Con 10))) True
  assert (Body (Smaller (Con (-4)) (Con 0))) True
  assert (Body (Smaller (Con (-8)) (Con (-6)))) True
  assert (Body (Smaller (Con 12) (Con 1))) False
  assert (Body (Smaller (Con 345) (Con (-5)))) False
  assert (Body (Smaller (Con 83) (Con (-40)))) False
  assert (Body (Smaller (Con 0) (Con 0))) False

  -- compound Formulas
  assert (Body (And (And (Con True) (Con True)) (Con False))) False
  assert (Body (Or (Or (Con True) (Con True)) (Con True))) True
  assert (Body (Or (And (Con False) (Con False)) (Con False))) False
  assert (Body (And (Con True) (And (Con True) (Con False)))) False
  assert (Body (Or (Or (Con True) (Con True)) (Con False))) True
  assert (Body (And (And (Con True) (Con True)) (Con True))) True
  assert (Body (And (Con True) (Or (Con False) (Con False)))) False
  assert (Body (Or (Or (Con True) (Con False)) (Con True))) True
  assert (Body (Or (Smaller (Con 1) (Con 1)) (Con True))) True
  assert (Body (And (Con True) (Smaller (Con 4) (Con 6)))) True
  assert (Body (Smaller (Plus (Con 4) (Con 12)) (Con 19))) True
  assert (Body (Or (Smaller (Con 4) (Con 2)) (Con True))) True

-- prints True if the Formula is saitsfiable; false otherwise
assert :: Eq a => Formula a -> Bool -> IO ()
assert formula result = putStrLn $ show $ satisfiable formula == result
