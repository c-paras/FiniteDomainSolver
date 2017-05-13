#!/usr/bin/runhaskell

{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Tests the solver for quantified Formulas.
-}

module TestSolverExists where

import Formula
import Solver

main :: IO ()
main = do

  -- example 1: truth values

  let f = Body (Con True)
  assert f [()]

  let f = Body (Con False)
  assert f [()]

  -- example 2: n < n + 1 and variants

  let f = Exists [1..10] $ \n ->
          Body $ n `Smaller` (n `Plus` Con 1)
  assert f [(1,()),(2,()),(3,()),(4,()),(5,()),(6,()),(7,()),(8,()),(9,()),(10,())]

  let f = Exists [1..10] $ \n ->
          Body $ n `Smaller` (n `Plus` Con (-1))
  assert f []

  let f = Exists [1..10] $ \n ->
          Body $ (n `Plus` Con 1) `Smaller` n
  assert f []

  let f = Exists [1..10] $ \n ->
          Body $ (n `Plus` Con (-1)) `Smaller` n
  assert f [(1,()),(2,()),(3,()),(4,()),(5,()),(6,()),(7,()),(8,()),(9,()),(10,())]

  let f = Exists [] $ \n ->
          Body $ (n `Plus` Con (-1)) `Smaller` n
  assert f [] -- de facto solution for an empty quantification (since n is a free variable)

  -- example 3: p \/ (0 < n) and variants

  let f = Exists [False, True] $ \p ->
          Exists [0..2] $ \n ->
          Body $ p `Or` (Con 0 `Smaller` n)
  assert f [(False,(1,())),(False,(2,())),(True,(0,())),(True,(1,())),(True,(2,()))]

  let f = Exists [False, True] $ \p ->
          Exists [0] $ \n ->
          Body $ p `Or` (Con 0 `Smaller` n)
  assert f [(True,(0,()))]

  let f = Exists [True] $ \p ->
          Exists [0] $ \n ->
          Body $ p `Or` (Con 0 `Smaller` n)
  assert f [(True,(0,()))]

  let f = Exists [False] $ \p ->
          Exists [0] $ \n ->
          Body $ p `Or` (Con 0 `Smaller` n)
  assert f []

  let f = Exists [False] $ \p ->
          Exists [-1] $ \n ->
          Body $ p `Or` (Con 0 `Smaller` n)
  assert f []

  let f = Exists [False, True] $ \p ->
          Exists [-5] $ \n ->
          Body $ p `Or` (Con 0 `Smaller` n)
  assert f [(True,(-5,()))]

  -- example 4: 3 quantified integers

  let f = Exists [0..2] $ \a ->
          Exists [5..8] $ \b ->
          Exists [8..9] $ \c ->
          Body $ (Plus a b) `Smaller` c
  assert f [(0,(5,(8,()))),(0,(5,(9,()))),(0,(6,(8,()))),(0,(6,(9,()))),(0,(7,(8,()))),(0,(7,(9,()))),(0,(8,(9,()))),
            (1,(5,(8,()))),(1,(5,(9,()))),(1,(6,(8,()))),(1,(6,(9,()))),(1,(7,(9,()))),(2,(5,(8,()))),(2,(5,(9,()))),(2,(6,(9,())))]

  let f = Exists [3..6] $ \a ->
          Exists [6..8] $ \b ->
          Exists [8..9] $ \c ->
          Body $ (Plus a b) `Smaller` c
  assert f []

  let f = Exists [7..18] $ \a ->
          Exists [16..17] $ \b ->
          Exists [12..29] $ \c ->
          Body $ (Plus a b) `Smaller` c
  assert f [(7,(16,(24,()))),(7,(16,(25,()))),(7,(16,(26,()))),(7,(16,(27,()))),(7,(16,(28,()))),(7,(16,(29,()))),(7,(17,(25,()))),
            (7,(17,(26,()))),(7,(17,(27,()))),(7,(17,(28,()))),(7,(17,(29,()))),(8,(16,(25,()))),(8,(16,(26,()))),(8,(16,(27,()))),
            (8,(16,(28,()))),(8,(16,(29,()))),(8,(17,(26,()))),(8,(17,(27,()))),(8,(17,(28,()))),(8,(17,(29,()))),(9,(16,(26,()))),
            (9,(16,(27,()))),(9,(16,(28,()))),(9,(16,(29,()))),(9,(17,(27,()))),(9,(17,(28,()))),(9,(17,(29,()))),(10,(16,(27,()))),
            (10,(16,(28,()))),(10,(16,(29,()))),(10,(17,(28,()))),(10,(17,(29,()))),(11,(16,(28,()))),(11,(16,(29,()))),(11,(17,(29,()))),(12,(16,(29,())))]

  let f = Exists [15] $ \a ->
          Exists [0..15] $ \b ->
          Exists [15] $ \c ->
          Body $ (Plus a b) `Smaller` c
  assert f []

  let f = Exists [12..15] $ \a ->
          Exists [9..22] $ \b ->
          Exists [20] $ \c ->
          Body $ (Plus a b) `Smaller` c
  assert f []

  -- example 5: many quantified variables
  -- THE LARGE RANGE OF QUANTIFICATION OF THESE VARIABLES WOULD TAKE TOO LONG TO COMPUTE

{-
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
-}

-- prints True if the solver returns the right solution set; False otherwise
assert :: Eq a => Formula a -> [a] -> IO ()
assert formula result = putStrLn $ show $ solutions formula == result
