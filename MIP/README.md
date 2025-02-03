# MIP

[![Hackage](https://img.shields.io/hackage/v/MIP.svg)](https://hackage.haskell.org/package/MIP)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/MIP.svg)](https://packdeps.haskellers.com/feed?needle=MIP)
[![License: BSD 3-Clause](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

Library for using Mixed Integer Programming (MIP) in Haskell.
This library contains functions like:

* Reading / Writing MIP problem files (e.g. `LP` file or `MPS` file),
* Invokling MIP solvers like Gurobi, CPLEX, CBC, GLPK, lp_solve,
* Reading solution files of those solvers.

## Examples

### Convert LP file into MPS file

```haskell
import qualified Numeric.Optimization.MIP as MIP

main :: IO ()
main = do
  prob <- MIP.readFile MIP.def "samples/lp/test.lp"
  MIP.writeFile MIP.def "test.mps" prob
```

If you want to convert problems between from/to other formats (e.g. SAT, Max-SAT, Pseudo Boolean, SMT, QUBO), please consult [toysolver](https://hackage.haskell.org/package/toysolver) package.

### Solve LP file using the CbC solver


```haskell
import Control.Monad
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T

import qualified Numeric.Optimization.MIP as MIP
import Numeric.Optimization.MIP.Solver

main :: IO ()
main = do
  prob <- MIP.readFile MIP.def "samples/lp/test.lp"
  sol <- solve cbc MIP.def{ solveTimeLimit = Just 10.0 } prob
  print $ MIP.solStatus sol
  putStrLn $ "Objective Value: " ++ show (MIP.solObjectiveValue sol)
  forM_ (MIP.variables prob) $ \v -> do
    putStrLn $ T.unpack (MIP.varName v) ++ " = " ++ show (MIP.solVariables sol Map.! v)
```

### Constructing a problem instance and solving it using the CbC solver

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T

import qualified Numeric.Optimization.MIP as MIP
import Numeric.Optimization.MIP ((.<=.))
import Numeric.Optimization.MIP.Solver

-- Example from https://en.wikipedia.org/wiki/Integer_programming
main :: IO ()
main = do
  let [x, y] = map MIP.varExpr ["x", "y"]
      prob =
        MIP.def
        { MIP.objectiveFunction =
            MIP.def
            { MIP.objDir = MIP.OptMax
            , MIP.objExpr = y
            }
        , MIP.constraints =
            [ - x +   y .<=. 1
            , 3*x + 2*y .<=. 12
            , 2*x + 3*y .<=. 12
            ]
        , MIP.varDomains =
            Map.fromList
            [ ("x", (MIP.IntegerVariable, (0, MIP.PosInf)))
            , ("y", (MIP.IntegerVariable, (0, MIP.PosInf)))
            ]
        }

  sol <- solve cbc MIP.def{ solveTimeLimit = Just 10.0 } prob
  print $ MIP.solStatus sol
  putStrLn $ "Objective Value: " ++ show (MIP.solObjectiveValue sol)
  forM_ (MIP.variables prob) $ \v -> do
    putStrLn $ T.unpack (MIP.varName v) ++ " = " ++ show (MIP.solVariables sol Map.! v)
```

If you want to solve problems without explicit formulation (e.g. black box optimization), please consult [numeric-optimization](https://hackage.haskell.org/package/numeric-optimization) package.
