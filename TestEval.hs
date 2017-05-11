#!/usr/bin/runhaskell

{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Tests the evaluation of logical terms.
-}

module TestEval where

import Formula
import Solver

main :: IO ()
main = do

  -- constant terms
  assert (Con True) True
  assert (Con False) False
  assert (Con 0) 0
  assert (Con 1) 1
  assert (Con 5) 5
  assert (Con 10) 10
  assert (Con (-3)) (-3)

  -- and terms
  assert (And (Con True) (Con True)) True
  assert (And (Con True) (Con False)) False
  assert (And (Con False) (Con True)) False
  assert (And (Con False) (Con False)) False

  -- or terms
  assert (Or (Con True) (Con True)) True
  assert (Or (Con True) (Con False)) True
  assert (Or (Con False) (Con True)) True
  assert (Or (Con False) (Con False)) False

  -- smaller terms
  assert (Smaller (Con 0) (Con 1)) True
  assert (Smaller (Con 5) (Con 10)) True
  assert (Smaller (Con (-4)) (Con 0)) True
  assert (Smaller (Con (-8)) (Con (-6))) True
  assert (Smaller (Con 12) (Con 1)) False
  assert (Smaller (Con 345) (Con (-5))) False
  assert (Smaller (Con 83) (Con (-40))) False
  assert (Smaller (Con 0) (Con 0)) False

  -- plus terms
  assert (Plus (Con 5) (Con 6)) 11
  assert (Plus (Con 5) (Con (-5))) 0
  assert (Plus (Con 24) (Con 8)) 32
  assert (Plus (Con (-23)) (Con (-15))) (-38)
  assert (Plus (Con 111) (Con 222)) 333
  assert (Plus (Con (-4)) (Con 14)) 10
  assert (Plus (Con 16) (Con 16)) 32
  assert (Plus (Con 0) (Con 0)) 0

  -- compound terms
  assert (And (And (Con True) (Con True)) (Con False)) False
  assert (Or (Or (Con True) (Con True)) (Con True)) True
  assert (Or (And (Con False) (Con False)) (Con False)) False
  assert (And (Con True) (And (Con True) (Con False))) False
  assert (Or (Or (Con True) (Con True)) (Con False)) True
  assert (And (And (Con True) (Con True)) (Con True)) True
  assert (And (Con True) (Or (Con False) (Con False))) False
  assert (Or (Or (Con True) (Con False)) (Con True)) True
  assert (Or (Smaller (Con 1) (Con 1)) (Con True)) True
  assert (And (Con True) (Smaller (Con 4) (Con 6))) True
  assert (Smaller (Plus (Con 4) (Con 12)) (Con 19)) True
  assert (Or (Smaller (Con 4) (Con 2)) (Con True)) True

-- prints True if the term evaluates to the expected result; false otherwise
assert :: Eq a => Term a -> a -> IO ()
assert term result = putStrLn $ show $ eval term == result
