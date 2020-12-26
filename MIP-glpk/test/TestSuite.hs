{-# OPTIONS_GHC -Wall -Wno-unused-top-binds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

import qualified Math.Programming.Glpk.Header as Raw
import qualified Numeric.Optimization.MIP as MIP
import Numeric.Optimization.MIP.Solver
import Numeric.Optimization.MIP.Solver.GLPK

-- ------------------------------------------------------------------------

case_glpk :: Assertion
case_glpk = do
  prob <- MIP.readFile MIP.def "samples/lp/test.lp"
  sol <- solve glpk MIP.def prob
  sol @?=
    MIP.Solution
    { MIP.solStatus = MIP.StatusOptimal
    , MIP.solObjectiveValue = Just 122.5
    , MIP.solVariables = Map.fromList [("x1", 40), ("x2", 10.5), ("x3", 19.5), ("x4", 3)]
    }

case_glpk_unbounded :: Assertion
case_glpk_unbounded = do
  prob <- MIP.readFile MIP.def "samples/lp/unbounded-ip.lp"
  sol <- solve glpk MIP.def prob
  let status = MIP.solStatus sol
  unless (status == MIP.StatusUnbounded || status == MIP.StatusFeasible || status == MIP.StatusInfeasibleOrUnbounded) $
    assertFailure $ unlines $
      [ "expected: StatusUnbounded, StatusFeasible or StatusInfeasibleOrUnbounded"
      , " but got: " ++ show status
      ]

case_glpk_infeasible :: Assertion
case_glpk_infeasible = do
  prob <- MIP.readFile MIP.def "samples/lp/infeasible.lp"
  sol <- solve glpk MIP.def prob
  MIP.solStatus sol @?= MIP.StatusInfeasible

-- ------------------------------------------------------------------------

case_glpk_thread_safe :: Assertion
case_glpk_thread_safe = when rtsSupportsBoundThreads $ do
  th1 <- asyncBound $ do
    Raw.glp_init_env >>= (@?= 0)
    Raw.glp_init_env >>= (@?= 1)
    th2 <- asyncBound $ do
      Raw.glp_init_env >>= (@?= 0)
      Raw.glp_init_env >>= (@?= 1)
      _ <- Raw.glp_free_env
      Raw.glp_init_env >>= (@?= 0)
    wait th2
    Raw.glp_init_env >>= (@?= 1)
    _ <- Raw.glp_free_env
    Raw.glp_init_env >>= (@?= 0)
  wait th1

-- ------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "MIP-glpk test suite"
  [ testCase "glpk" case_glpk
  , testCase "glpk unbounded" case_glpk_unbounded
  , testCase "glpk infeasible" case_glpk_infeasible
  , testCase "glpk thread safe" case_glpk_thread_safe
  ]
