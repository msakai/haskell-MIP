# MIP

[![Hackage](https://img.shields.io/hackage/v/MIP.svg)](https://hackage.haskell.org/package/MIP)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/MIP.svg)](https://packdeps.haskellers.com/feed?needle=MIP)
[![License: BSD 3-Clause](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

Library for using Mixed Integer Programming (MIP) in Haskell.
This library contains functions like:

* Reading / Writing MIP problem files (e.g. `LP` file or `MPS` file),
* Invokling MIP solvers like Gurobi, CPLEX, CBC, GLPK, lp_solve,
* Reading solution files of those solvers.
