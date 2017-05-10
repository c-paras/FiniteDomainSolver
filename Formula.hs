{-# LANGUAGE GADTs #-}

{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Defines logical formulas and the terms they consist of.
-}

module Formula where

-- logical formula indexed by a list of the types of its existentially-quantified variables
data Formula ts where
  Body   :: Term Bool                     -> Formula ()
  Exists :: Show a
         => [a] -> (Term a -> Formula as) -> Formula (a, as)

-- terms that form logical formulas indexed by their type
data Term t where
  Con     :: a                      -> Term a
  And     :: Term Bool -> Term Bool -> Term Bool
  Or      :: Term Bool -> Term Bool -> Term Bool
  Smaller :: Term Int  -> Term Int  -> Term Bool
  Plus    :: Term Int  -> Term Int  -> Term Int
  Name    :: String    -> Term t -- to facilitate pretty printing

-- pretty printing for a term
instance Show t => Show (Term t) where
  show (Con v)       = show v
  show (And p q)     = "(" ++ show p ++ " && " ++ show q ++ ")"
  show (Or p q)      = "(" ++ show p ++ " || " ++ show q ++ ")"
  show (Smaller n m) = "(" ++ show n ++ " < "  ++ show m ++ ")"
  show (Plus n m)    = "(" ++ show n ++ " + "  ++ show m ++ ")"
  show (Name name)   = name

-- pretty printing for a formula
instance Show (Formula ts) where
  show = show' ['x' : show i | i <- [0..]]
    where
      show' :: [String] -> Formula ts' -> String
      show' ns     (Body body)   = show body
      show' (n:ns) (Exists vs p) = "exists " ++ n ++ "::" ++ show vs ++ ". " ++ show' ns (p (Name n))

-- some example formulas
ex1 :: Formula ()
ex1 = Body (Con True)
ex2 :: Formula (Int, ())
ex2 = Exists [1..10] $ \n ->
        Body $ n `Smaller` (n `Plus` Con 1)
ex3 :: Formula (Bool, (Int, ()))
ex3 = Exists [False, True] $ \p ->
      Exists [0..2] $ \n ->
        Body $ p `Or` (Con 0 `Smaller` n)
