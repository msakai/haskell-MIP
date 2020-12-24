-- {-# OPTIONS_GHC -Wall -Wno-unused-top-binds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad
import Data.Default.Class
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit
import qualified Numeric.Optimization.MIP as MIP
import Numeric.Optimization.MIP.Solver
import Numeric.Optimization.MIP.Solver.GLPK

-- ------------------------------------------------------------------------

case_glpk :: Assertion
case_glpk = do
  prob <- MIP.readFile def "samples/lp/test.lp"
  sol <- solve glpk def prob
  sol @?=
    MIP.Solution
    { MIP.solStatus = MIP.StatusOptimal
    , MIP.solObjectiveValue = Just 122.5
    , MIP.solVariables = Map.fromList [("x1", 40), ("x2", 10.5), ("x3", 19.5), ("x4", 3)]
    }

case_glpk_unbounded :: Assertion
case_glpk_unbounded = do
  prob <- MIP.readFile def "samples/lp/unbounded-ip.lp"
  sol <- solve glpk def prob
  let status = MIP.solStatus sol
  unless (status == MIP.StatusUnbounded || status == MIP.StatusFeasible || status == MIP.StatusInfeasibleOrUnbounded) $
    assertFailure $ unlines $
      [ "expected: StatusUnbounded, StatusFeasible or StatusInfeasibleOrUnbounded"
      , " but got: " ++ show status
      ]

case_glpk_infeasible :: Assertion
case_glpk_infeasible = do
  prob <- MIP.readFile def "samples/lp/infeasible.lp"
  sol <- solve glpk def prob
  MIP.solStatus sol @?= MIP.StatusInfeasible

-- ------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "MIP-glpk test suite"
  [ testCase "glpk" case_glpk
  , testCase "glpk unbounded" case_glpk_unbounded
  , testCase "glpk infeasible" case_glpk_infeasible
  ]