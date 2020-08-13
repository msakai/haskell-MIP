module Main where

import Test.Tasty (defaultMain, testGroup)

import Test.LPFile
import Test.MIP
import Test.MIPSolver
import Test.MPSFile

main :: IO ()
main = defaultMain $ testGroup "ToySolver test suite"
  [ lpTestGroup
  , mipTestGroup
  , mipSolverTestGroup
  , mpsTestGroup
  ]
