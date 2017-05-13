# FiniteDomainSolver
Implements a solver for simple logical formulas

This application was built and tested on Ubuntu 16.04 LTS using GHC version 7.10.3.

Logical Terms and Formulas are typed and are represented using GADT syntax. Terms are usually constructed using prefix notation as in `And (Con True) (Smaller (Con 4) (Con 6))`. A Formula may be unquantified, in which case it consists only of a Term with a Bool type or may contain one or more quantified variables. Some examples are provided at the end of Formula.hs.

A type-safe evaluator `eval` is implemented in Solver.hs.

Solver.hs also implements a satisfiability checker `satisfiable` for arbitrary Formulas and a solver `solutions` which computes all possible solutions of a Formula.

A comprehensive test suite is provided for the evaluator, satisfiability checker and solver.

To run the evaluator tests:
```
./TestEval.hs
```
To run the satisfiability checker tests for unquantified Formulas:
```
./TestSatisfiabilityBody.hs
```
To run the satisfiability checker tests for quantified Formulas:
```
./TestSatisfiabilityExists.hs
```
To run the solver tests for unquantified Formulas:
```
./TestSolverBody.hs
```
To run the solver tests for quantified Formulas:
```
./TestSolverExists.hs
```

Copyright (C) 2017 Costa Paraskevopoulos

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
