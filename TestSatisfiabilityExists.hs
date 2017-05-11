#!/usr/bin/runhaskell

{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Tests the satisfiability checking of quantified Formulas.
-}

module TestSatisfiablyExists where

import Formula
import Solver

main :: IO ()
main = do

  -- example 1: truth values

  let f = Body (Con True)
  assert f True

  let f = Body (Con False)
  assert f False

  -- example 2: n < n + 1 and variants

  let f = Exists [1..10] $ \n ->
          Body $ n `Smaller` (n `Plus` Con 1)
  assert f True

  let f = Exists [1..10] $ \n ->
          Body $ n `Smaller` (n `Plus` Con (-1))
  assert f False

  let f = Exists [1..10] $ \n ->
          Body $ (n `Plus` Con 1) `Smaller` n
  assert f False

  let f = Exists [1..10] $ \n ->
          Body $ (n `Plus` Con (-1)) `Smaller` n
  assert f True

  let f = Exists [] $ \n ->
          Body $ (n `Plus` Con (-1)) `Smaller` n
  assert f False -- de facto satisfiability for an empty quantification (since n is a free variable)

  -- example 3: p \/ (0 < n) and variants

  let f = Exists [False, True] $ \p ->
          Exists [0..2] $ \n ->
          Body $ p `Or` (Con 0 `Smaller` n)
  assert f True

  let f = Exists [False, True] $ \p ->
          Exists [0] $ \n ->
          Body $ p `Or` (Con 0 `Smaller` n)
  assert f True

  let f = Exists [True] $ \p ->
          Exists [0] $ \n ->
          Body $ p `Or` (Con 0 `Smaller` n)
  assert f True

  let f = Exists [False] $ \p ->
          Exists [0] $ \n ->
          Body $ p `Or` (Con 0 `Smaller` n)
  assert f False

  let f = Exists [False] $ \p ->
          Exists [-1] $ \n ->
          Body $ p `Or` (Con 0 `Smaller` n)
  assert f False

  let f = Exists [False, True] $ \p ->
          Exists [-5] $ \n ->
          Body $ p `Or` (Con 0 `Smaller` n)
  assert f True

  -- example 4: 3 quantified integers

  let f = Exists [0..2] $ \a ->
          Exists [5..8] $ \b ->
          Exists [8..9] $ \c ->
          Body $ (Plus a b) `Smaller` c
  assert f True

  let f = Exists [3..6] $ \a ->
          Exists [6..8] $ \b ->
          Exists [8..9] $ \c ->
          Body $ (Plus a b) `Smaller` c
  assert f False

  let f = Exists [7..18] $ \a ->
          Exists [16..17] $ \b ->
          Exists [12..29] $ \c ->
          Body $ (Plus a b) `Smaller` c
  assert f True

  let f = Exists [15] $ \a ->
          Exists [0..15] $ \b ->
          Exists [15] $ \c ->
          Body $ (Plus a b) `Smaller` c
  assert f False

  let f = Exists [12..15] $ \a ->
          Exists [9..22] $ \b ->
          Exists [20] $ \c ->
          Body $ (Plus a b) `Smaller` c
  assert f False

  -- example 5: many quantified variables

  let f = Exists [True, False] $ \a ->
          Exists [-4..5] $ \b ->
          Exists [2..6] $ \c ->
          Exists [124..344] $ \d ->
          Exists [12..44] $ \e ->
          Body $ And a ((Plus b c) `Smaller` (Plus d e))
  assert f True

  let f = Exists [True, False] $ \a ->
          Exists [124..344] $ \b ->
          Exists [12..44] $ \c ->
          Exists [-4..5] $ \d ->
          Exists [2..6] $ \e ->
          Body $ And a ((Plus b c) `Smaller` (Plus d e))
  assert f False

  let f = Exists [False] $ \a ->
          Exists [-4..5] $ \b ->
          Exists [2..6] $ \c ->
          Exists [124..344] $ \d ->
          Exists [12..44] $ \e ->
          Body $ And a ((Plus b c) `Smaller` (Plus d e))
  assert f False

  let f = Exists [True, False] $ \a ->
          Exists [20..22] $ \b ->
          Exists [20..22] $ \c ->
          Exists [-50..21] $ \d ->
          Exists [-100..20] $ \e ->
          Body $ And a ((Plus b c) `Smaller` (Plus d e))
  assert f True

  let f = Exists [False] $ \a ->
          Exists [50] $ \b ->
          Exists [50] $ \c ->
          Exists [-674..0] $ \d ->
          Exists [-999..101] $ \e ->
          Body $ Or a ((Plus b c) `Smaller` (Plus d e))
  assert f True

  let f = Exists [True] $ \a ->
          Exists [-40..40] $ \b ->
          Exists [214..236] $ \c ->
          Exists [-99..50] $ \d ->
          Exists [87..103] $ \e ->
          Body $ And a ((Plus b c) `Smaller` (Plus d e))
  assert f False

-- prints True if the Formula is saitsfiable; False otherwise
assert :: Eq a => Formula a -> Bool -> IO ()
assert formula result = putStrLn $ show $ satisfiable formula == result
