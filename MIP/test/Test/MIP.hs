{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.MIP (mipTestGroup) where

import Algebra.PartialOrd
#if !MIN_VERSION_lattices(2,0,0)
import Algebra.Lattice
#endif
import Data.Maybe
import qualified Data.Map as Map
import Test.QuickCheck.Instances.Text ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Numeric.Optimization.MIP (meetStatus)
import qualified Numeric.Optimization.MIP as MIP
import qualified Numeric.Optimization.MIP.Solution.CBC as CBCSol
import qualified Numeric.Optimization.MIP.Solution.CPLEX as CPLEXSol
import qualified Numeric.Optimization.MIP.Solution.GLPK as GLPKSol
import qualified Numeric.Optimization.MIP.Solution.Gurobi as GurobiSol
import qualified Numeric.Optimization.MIP.Solution.HiGHS as HiGHSSol
import qualified Numeric.Optimization.MIP.Solution.Printemps as PrintempsSol
import qualified Numeric.Optimization.MIP.Solution.SCIP as SCIPSol

case_var_show :: Assertion
case_var_show = show (MIP.Var "x") @?= show ("x" :: String)

prop_var_name :: Property
prop_var_name =
  forAll arbitrary $ \x -> do
    MIP.varName (MIP.Var x) === x

prop_var_compare :: Property
prop_var_compare =
  forAll arbitrary $ \(x1, x2) -> do
    compare (MIP.Var x1) (MIP.Var x2) === compare x1 x2

prop_status_refl :: Property
prop_status_refl = forAll arbitrary $ \(x :: MIP.Status) -> do
  x `leq` x

prop_status_trans :: Property
prop_status_trans =
  forAll arbitrary $ \(x :: MIP.Status) ->
    forAll (upper x) $ \y ->
      forAll (upper y) $ \z ->
        x `leq` z
  where
    -- upper :: (PartialOrd a, Enum a, Bounded a) => a -> Gen a
    upper a = elements [b | b <- [minBound .. maxBound], a `leq` b]

prop_status_meet_idempotency :: Property
prop_status_meet_idempotency =
  forAll arbitrary $ \(x :: MIP.Status) ->
    x `meetStatus` x == x

prop_status_meet_comm :: Property
prop_status_meet_comm =
  forAll arbitrary $ \(x :: MIP.Status) y ->
    x `meetStatus` y == y `meetStatus` x

prop_status_meet_assoc :: Property
prop_status_meet_assoc =
  forAll arbitrary $ \(x :: MIP.Status) y z ->
    (x `meetStatus` y) `meetStatus` z == x `meetStatus` (y `meetStatus` z)

prop_status_meet_leq :: Property
prop_status_meet_leq =
  forAll arbitrary $ \(x :: MIP.Status) y ->
#if MIN_VERSION_lattices(2,0,0)
    (x == (x `meetStatus` y)) == x `leq` y
#else
    x `meetLeq` y == x `leq` y
#endif

instance Arbitrary MIP.Status where
  arbitrary = arbitraryBoundedEnum

case_CBCSol :: Assertion
case_CBCSol = do
  sol <- CBCSol.readFile "samples/lp/test-solution-cbc.txt"
  sol @?=
    MIP.Solution
    { MIP.solStatus = MIP.StatusOptimal
    , MIP.solObjectiveValue = Just (-122.5)
    , MIP.solVariables = Map.fromList [("x1", 40), ("x2", 10.5), ("x3", 19.5), ("x4", 3)]
    }

case_CBCSol_infeasible :: Assertion
case_CBCSol_infeasible = do
  sol <- CBCSol.readFile "samples/lp/test-solution-cbc-infeasible.txt"
  sol @?=
    MIP.Solution
    { MIP.solStatus = MIP.StatusInfeasible
    , MIP.solObjectiveValue = Just 0.00000000
    , MIP.solVariables = Map.fromList [("x", 0.11111111), ("y", 0), ("z", 0.33333333)]
    }

case_CBCSol_unbounded :: Assertion
case_CBCSol_unbounded = do
  sol <- CBCSol.readFile "samples/lp/test-solution-cbc-unbounded.txt"
  sol @?=
    MIP.Solution
    { MIP.solStatus = MIP.StatusInfeasibleOrUnbounded
    , MIP.solObjectiveValue = Just 0.00000000
    , MIP.solVariables = Map.fromList [("x", 0), ("y", 0)]
    }

case_CPLEXSol :: Assertion
case_CPLEXSol = do
  sol <- CPLEXSol.readFile "samples/lp/test-solution-cplex.sol"
  sol @?=
    MIP.Solution
    { MIP.solStatus = MIP.StatusOptimal
    , MIP.solObjectiveValue = Just 122.5
    , MIP.solVariables = Map.fromList [("x1", 40), ("x2", 10.5), ("x3", 19.5), ("x4", 3)]
    }

case_CPLEXSol_unbounded :: Assertion
case_CPLEXSol_unbounded = do
  sol <- CPLEXSol.readFile "samples/lp/test-solution-cplex-unbounded.sol"
  sol @?=
    MIP.Solution
    { MIP.solStatus = MIP.StatusUnbounded
    , MIP.solObjectiveValue = Just 3.0
    , MIP.solVariables = Map.fromList [("x", 1.0), ("y", 2.0)]
    }

case_GLPKSol :: Assertion
case_GLPKSol = do
  sol <- GLPKSol.readFile "samples/lp/test-solution-glpk.sol"
  sol @?=
    MIP.Solution
    { MIP.solStatus = MIP.StatusOptimal
    , MIP.solObjectiveValue = Just 122.5
    , MIP.solVariables = Map.fromList [("x1", 40), ("x2", 10.5), ("x3", 19.5), ("x4", 3)]
    }

case_GLPKSol_long_var :: Assertion
case_GLPKSol_long_var = do
  sol <- GLPKSol.readFile "samples/lp/test-solution-glpk-long.sol"
  sol @?=
    MIP.Solution
    { MIP.solStatus = MIP.StatusOptimal
    , MIP.solObjectiveValue = Just 122.5
    , MIP.solVariables = Map.fromList [("x1AAAAAAAAAAAAAAAAAAAAAAAAAAAA", 40), ("x2", 10.5), ("x3", 19.5), ("x4", 3)]
    }

case_GurobiSol :: Assertion
case_GurobiSol = do
  sol <- GurobiSol.readFile "samples/lp/test-solution-gurobi.sol"
  isJust (GurobiSol.solObjectiveValue sol) @?= True
  GurobiSol.parse (GurobiSol.render sol) @?= sol

case_HiGHSSol :: Assertion
case_HiGHSSol = do
  sol <- HiGHSSol.readFile "samples/lp/test-solution-highs.sol"
  sol @?=
    MIP.Solution
    { MIP.solStatus = MIP.StatusOptimal
    , MIP.solObjectiveValue = Just 122.5
    , MIP.solVariables = Map.fromList [("x1", 40), ("x2", 10.5), ("x3", 19.5), ("x4", 3)]
    }

case_HiGHSSol_infeasible :: Assertion
case_HiGHSSol_infeasible = do
  sol <- HiGHSSol.readFile "samples/lp/test-solution-highs-infeasible.sol"
  sol @?=
    MIP.Solution
    { MIP.solStatus = MIP.StatusInfeasible
    , MIP.solObjectiveValue = Nothing
    , MIP.solVariables = Map.empty
    }

case_HiGHSSol_unbounded :: Assertion
case_HiGHSSol_unbounded = do
  sol <- HiGHSSol.readFile "samples/lp/test-solution-highs-unbounded.sol"
  sol @?=
    MIP.Solution
    { MIP.solStatus = MIP.StatusInfeasibleOrUnbounded
    , MIP.solObjectiveValue = Nothing
    , MIP.solVariables = Map.empty
    }

case_PrintempsSol :: Assertion
case_PrintempsSol = do
  sol <- PrintempsSol.readFile "samples/lp/test-solution-printemps.json"
  sol @?=
    MIP.Solution
    { MIP.solStatus = MIP.StatusFeasible
    , MIP.solObjectiveValue = Just (-1.070000e+02)
    , MIP.solVariables = Map.fromList [("x1", 29), ("x2", 7), ("x3", 22), ("x4", 2)]
    }

case_PrintempsSol_infeasible :: Assertion
case_PrintempsSol_infeasible = do
  sol <- PrintempsSol.readFile "samples/lp/test-solution-printemps-infeasible.json"
  sol @?=
    MIP.Solution
    { MIP.solStatus = MIP.StatusUnknown
    , MIP.solObjectiveValue = Just 0
    , MIP.solVariables = Map.fromList [("x", 0), ("y", 0)]
    }

case_SCIPSol :: Assertion
case_SCIPSol = do
  sol <- SCIPSol.readFile "samples/lp/test-solution-scip.sol"
  sol @?=
    MIP.Solution
    { MIP.solStatus = MIP.StatusOptimal
    , MIP.solObjectiveValue = Just 122.5
    , MIP.solVariables = Map.fromList [("x1", 40), ("x2", 10.5), ("x3", 19.5), ("x4", 3)]
    }

mipTestGroup :: TestTree
mipTestGroup = $(testGroupGenerator)
