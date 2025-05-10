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
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import System.FilePath ((</>))
import System.IO
import System.IO.Temp
import Test.QuickCheck.Instances.Text ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Numeric.Optimization.MIP (meetStatus)
import qualified Numeric.Optimization.MIP as MIP
import qualified Numeric.Optimization.MIP.Base as Base
import qualified Numeric.Optimization.MIP.Solution.CBC as CBCSol
import qualified Numeric.Optimization.MIP.Solution.CPLEX as CPLEXSol
import qualified Numeric.Optimization.MIP.Solution.GLPK as GLPKSol
import qualified Numeric.Optimization.MIP.Solution.Gurobi as GurobiSol
import qualified Numeric.Optimization.MIP.Solution.HiGHS as HiGHSSol
import qualified Numeric.Optimization.MIP.Solution.MIPLIB as MIPLIBSol
import qualified Numeric.Optimization.MIP.Solution.Printemps as PrintempsSol
import qualified Numeric.Optimization.MIP.Solution.SCIP as SCIPSol

#ifdef WITH_ZLIB
import qualified Codec.Compression.GZip as GZip
#endif


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

case_sos_pattern_synonym_pattern_match :: Assertion
case_sos_pattern_synonym_pattern_match = do
  case MIP.S1 of
    MIP.SOS1 -> pure ()
    MIP.SOS2 -> assertFailure "SOS2 should not match S1"
  case MIP.S2 of
    MIP.SOS1 -> assertFailure "SOS1 should not match S2"
    MIP.SOS2 -> pure ()

  case MIP.SOS1 of
    MIP.S1 -> pure ()
    MIP.S2 -> assertFailure "S2 should not match SOS1"
  case MIP.SOS2 of
    MIP.S1 -> assertFailure "S1 should not match SOS2"
    MIP.S2 -> pure ()

case_eval_expr :: Assertion
case_eval_expr = do
  MIP.eval MIP.def sol (MIP.varExpr "x" + 2 * MIP.varExpr "y" :: MIP.Expr Rational) @?= 8
  where
    sol :: Map.Map MIP.Var Rational
    sol = Map.fromList [("x", 2), ("y", 3)]

case_eval_constraint :: Assertion
case_eval_constraint = do
  let constr1 = MIP.varExpr "x" MIP..<=. MIP.constExpr (0 :: Double)
  MIP.eval MIP.def (Map.singleton "x" (-1 :: Double)) constr1 @?= True
  MIP.eval MIP.def (Map.singleton "x" (0 :: Double)) constr1 @?= True
  MIP.eval MIP.def (Map.singleton "x" (1e-10 :: Double)) constr1 @?= True
  MIP.eval MIP.def (Map.singleton "x" (1 :: Double)) constr1 @?= False

  let constr2 :: MIP.Constraint Double
      constr2 = MIP.def{ MIP.constrExpr = MIP.varExpr "x", MIP.constrLB = MIP.NegInf, MIP.constrUB = MIP.PosInf }
  MIP.eval MIP.def (Map.singleton "x" (0 :: Double)) constr2 @?= True
  MIP.eval MIP.def (Map.singleton "x" (1 :: Double)) constr2 @?= True

  let constr3 :: MIP.Constraint Double
      constr3 = MIP.def{ MIP.constrIndicator = Just ("y", 0), MIP.constrExpr = MIP.varExpr "x", MIP.constrLB = MIP.Finite 0, MIP.constrUB = MIP.Finite 1 }
  MIP.eval MIP.def (Map.fromList [("x", 0 :: Double), ("y", 0)]) constr3 @?= True
  MIP.eval MIP.def (Map.fromList [("x", 2 :: Double), ("y", 0)]) constr3 @?= False
  MIP.eval MIP.def (Map.fromList [("x", 2 :: Double), ("y", 1)]) constr3 @?= True

case_eval_sos_constraint :: Assertion
case_eval_sos_constraint = do
  let constr1 :: MIP.SOSConstraint Double
      constr1 =
        MIP.SOSConstraint
        { MIP.sosLabel = Nothing
        , MIP.sosType = MIP.SOS1
        , MIP.sosBody = [("x1", 1), ("x2", 2), ("x3", 3)]
        }
  MIP.eval MIP.def (Map.fromList [("x1", 0 :: Double), ("x2", 0), ("x3", 0)]) constr1 @?= True
  MIP.eval MIP.def (Map.fromList [("x1", 1 :: Double), ("x2", 0), ("x3", 0)]) constr1 @?= True
  MIP.eval MIP.def (Map.fromList [("x1", 1 :: Double), ("x2", 1), ("x3", 0)]) constr1 @?= False

  let constr2 = constr1{ MIP.sosType = MIP.SOS2 }
  MIP.eval MIP.def (Map.fromList [("x1", 0 :: Double), ("x2", 0), ("x3", 0)]) constr2 @?= True
  MIP.eval MIP.def (Map.fromList [("x1", 1 :: Double), ("x2", 0), ("x3", 0)]) constr2 @?= True
  MIP.eval MIP.def (Map.fromList [("x1", 1 :: Double), ("x2", 1), ("x3", 0)]) constr2 @?= True
  MIP.eval MIP.def (Map.fromList [("x1", 0 :: Double), ("x2", 1), ("x3", 1)]) constr2 @?= True
  MIP.eval MIP.def (Map.fromList [("x1", 1 :: Double), ("x2", 0), ("x3", 1)]) constr2 @?= False
  MIP.eval MIP.def (Map.fromList [("x1", 1 :: Double), ("x2", 1), ("x3", 1)]) constr2 @?= False

case_eval_continuous_variable :: Assertion
case_eval_continuous_variable = do
  MIP.eval MIP.def (Map.singleton "x" (0 :: Double)) prob @?= Nothing
  MIP.eval MIP.def (Map.singleton "x" (1 - 1e-10 :: Double)) prob @?= Just 0
  MIP.eval MIP.def (Map.singleton "x" (1.5 :: Double)) prob @?= Just 0
  MIP.eval MIP.def (Map.singleton "x" (2 + 1e-10 :: Double)) prob @?= Just 0
  MIP.eval MIP.def (Map.singleton "x" (3 :: Double)) prob @?= Nothing
  where
    prob :: MIP.Problem Double
    prob =
      MIP.def
      { MIP.varDomains = Map.fromList [("x", (MIP.ContinuousVariable, (MIP.Finite 1, MIP.Finite 2)))]
      }

case_eval_semi_continuous_variable :: Assertion
case_eval_semi_continuous_variable = do
  MIP.eval MIP.def (Map.singleton "x" (0 - 1e-10 :: Double)) prob @?= Just 0
  MIP.eval MIP.def (Map.singleton "x" (0 + 1e-10 :: Double)) prob @?= Just 0
  MIP.eval MIP.def (Map.singleton "x" (0.5 :: Double)) prob @?= Nothing
  MIP.eval MIP.def (Map.singleton "x" (1 - 1e-10 :: Double)) prob @?= Just 0
  MIP.eval MIP.def (Map.singleton "x" (1.5 :: Double)) prob @?= Just 0
  MIP.eval MIP.def (Map.singleton "x" (2 + 1e-10 :: Double)) prob @?= Just 0
  MIP.eval MIP.def (Map.singleton "x" (3 :: Double)) prob @?= Nothing
  where
    prob :: MIP.Problem Double
    prob =
      MIP.def
      { MIP.varDomains = Map.fromList [("x", (MIP.SemiContinuousVariable, (MIP.Finite 1, MIP.Finite 2)))]
      }

case_eval_integer_variable :: Assertion
case_eval_integer_variable = do
  MIP.eval MIP.def (Map.singleton "x" (0 - 1e-10 :: Double)) prob @?= Nothing
  MIP.eval MIP.def (Map.singleton "x" (0 + 1e-10 :: Double)) prob @?= Nothing
  MIP.eval MIP.def (Map.singleton "x" (0.5 :: Double)) prob @?= Nothing
  MIP.eval MIP.def (Map.singleton "x" (1 - 1e-10 :: Double)) prob @?= Just 0
  MIP.eval MIP.def (Map.singleton "x" (1.5 :: Double)) prob @?= Nothing
  MIP.eval MIP.def (Map.singleton "x" (2 + 1e-10 :: Double)) prob @?= Just 0
  MIP.eval MIP.def (Map.singleton "x" (3 :: Double)) prob @?= Nothing
  where
    prob :: MIP.Problem Double
    prob =
      MIP.def
      { MIP.varDomains = Map.fromList [("x", (MIP.IntegerVariable, (MIP.Finite 1, MIP.Finite 2)))]
      }

case_eval_semi_integer_variable :: Assertion
case_eval_semi_integer_variable = do
  MIP.eval MIP.def (Map.singleton "x" (0 - 1e-10 :: Double)) prob @?= Just 0
  MIP.eval MIP.def (Map.singleton "x" (0 + 1e-10 :: Double)) prob @?= Just 0
  MIP.eval MIP.def (Map.singleton "x" (0.5 :: Double)) prob @?= Nothing
  MIP.eval MIP.def (Map.singleton "x" (1 - 1e-10 :: Double)) prob @?= Just 0
  MIP.eval MIP.def (Map.singleton "x" (1.5 :: Double)) prob @?= Nothing
  MIP.eval MIP.def (Map.singleton "x" (2 + 1e-10 :: Double)) prob @?= Just 0
  MIP.eval MIP.def (Map.singleton "x" (3 :: Double)) prob @?= Nothing
  where
    prob :: MIP.Problem Double
    prob =
      MIP.def
      { MIP.varDomains = Map.fromList [("x", (MIP.SemiIntegerVariable, (MIP.Finite 1, MIP.Finite 2)))]
      }

asciiProblem :: MIP.Problem Rational
asciiProblem = MIP.def
  { MIP.name = Just "problem"
  , MIP.objectiveFunction = MIP.def{ MIP.objLabel = Just "obj" }
  , MIP.constraints = [MIP.def{ MIP.constrLabel = Just "c1", MIP.constrExpr = MIP.varExpr "x1", MIP.constrUB = 1 }]
  , MIP.userCuts = [MIP.def{ MIP.constrLabel = Just "u1", MIP.constrExpr = MIP.varExpr "x2", MIP.constrUB = 1 }]
  , MIP.sosConstraints = [MIP.def{ MIP.sosLabel = Just "s1", MIP.sosBody = [("x1", 1), ("x2", 2)] }]
  , MIP.varDomains = Map.fromList
     [ ("x1", (MIP.ContinuousVariable, (0, 1000)))
     , ("x2", (MIP.ContinuousVariable, (0, 1000)))
     ]
  }

case_isAscii :: Assertion
case_isAscii = do
  True @=? Base.isAscii asciiProblem
  False @=? Base.isAscii asciiProblem{ MIP.name = Just "🐱" }
  False @=? Base.isAscii asciiProblem{ MIP.objectiveFunction = MIP.def{ MIP.objLabel = Just "🐱"  } }
  False @=? Base.isAscii asciiProblem{ MIP.constraints = [MIP.def{ MIP.constrLabel = Just "🐱", MIP.constrExpr = MIP.varExpr "x1", MIP.constrUB = 1 }] }
  False @=? Base.isAscii asciiProblem{ MIP.userCuts = [MIP.def{ MIP.constrLabel = Just "🐱", MIP.constrExpr = MIP.varExpr "x2", MIP.constrUB = 1 }] }
  False @=? Base.isAscii asciiProblem{ MIP.sosConstraints = [MIP.def{ MIP.sosLabel = Just "🐱", MIP.sosBody = [("x1", 1), ("x2", 2)] }] }
  False @=? Base.isAscii asciiProblem{ MIP.varDomains = Map.insert "🐱" (MIP.IntegerVariable, (0, 1000)) (MIP.varDomains asciiProblem) }

case_file_io_lp :: Assertion
case_file_io_lp = do
  let opt = MIP.def{ MIP.optFileEncoding = Just utf8 }
  prob <- MIP.readFile opt "samples/lp/test.lp"
  withSystemTempDirectory "MIP" $ \dir -> do
    MIP.writeFile opt (dir </> "test.lp") prob
    prob2 <- MIP.readFile opt (dir </> "test.lp")
    prob2 @?= prob
    s <- BL.readFile (dir </> "test.lp")
    case nativeNewline of
      CRLF -> isCRLFByteString s @?= True
      LF -> isLFByteString s @?= True

    MIP.writeFile opt{ MIP.optNewline = Just LF } (dir </> "test_lf.lp") prob
    prob_lf <- MIP.readFile opt (dir </> "test_lf.lp")
    prob_lf @?= prob
    s_lf <- BL.readFile (dir </> "test_lf.lp")
    isLFByteString s_lf @?= True

    MIP.writeFile opt{ MIP.optNewline = Just CRLF } (dir </> "test_crlf.lp") prob
    prob_crlf <- MIP.readFile opt (dir </> "test_crlf.lp")
    prob_crlf @?= prob
    s_crlf <- BL.readFile (dir </> "test_crlf.lp")
    isCRLFByteString s_crlf @?= True

#ifdef WITH_ZLIB
    MIP.writeFile opt (dir </> "test.lp.gz") prob
    prob_gz <- MIP.readFile opt (dir </> "test.lp.gz")
    prob_gz @?= prob
    s_gz <- GZip.decompress <$> BL.readFile (dir </> "test.lp.gz")
    case nativeNewline of
      CRLF -> isCRLFByteString s_gz @?= True
      LF -> isLFByteString s_gz @?= True

    MIP.writeFile opt{ MIP.optNewline = Just LF } (dir </> "test_lf.lp.gz") prob
    prob_lf_gz <- MIP.readFile opt (dir </> "test_lf.lp.gz")
    prob_lf_gz @?= prob
    s_lf_gz <- GZip.decompress <$> BL.readFile (dir </> "test_lf.lp.gz")
    isLFByteString s_lf_gz @?= True

    MIP.writeFile opt{ MIP.optNewline = Just CRLF } (dir </> "test_crlf.lp.gz") prob
    prob_crlf_gz <- MIP.readFile opt (dir </> "test_crlf.lp.gz")
    prob_crlf_gz @?= prob
    s_crlf_gz <- GZip.decompress <$> BL.readFile (dir </> "test_crlf.lp.gz")
    isCRLFByteString s_crlf_gz @?= True
#endif

case_file_io_mps :: Assertion
case_file_io_mps = do
  let opt = MIP.def{ MIP.optFileEncoding = Just utf8 }
  prob <- MIP.readFile opt "samples/lp/test.lp"
  withSystemTempDirectory "MIP" $ \dir -> do
    MIP.writeFile opt (dir </> "test.mps") prob
    prob2 <- MIP.readFile opt (dir </> "test.mps")
    prob2 @?= prob
    s <- BL.readFile (dir </> "test.mps")
    case nativeNewline of
      CRLF -> isCRLFByteString s @?= True
      LF -> isLFByteString s @?= True

    MIP.writeFile opt{ MIP.optNewline = Just LF } (dir </> "test_lf.mps") prob
    prob_lf <- MIP.readFile opt (dir </> "test_lf.mps")
    prob_lf @?= prob
    s_lf <- BL.readFile (dir </> "test_lf.mps")
    isLFByteString s_lf @?= True

    MIP.writeFile opt{ MIP.optNewline = Just CRLF } (dir </> "test_crlf.mps") prob
    prob_crlf <- MIP.readFile opt (dir </> "test_crlf.mps")
    prob_crlf @?= prob
    s_crlf <- BL.readFile (dir </> "test_crlf.mps")
    isCRLFByteString s_crlf @?= True

#ifdef WITH_ZLIB
    MIP.writeFile opt (dir </> "test.mps.gz") prob
    prob3 <- MIP.readFile opt (dir </> "test.mps.gz")
    prob3 @?= prob

    s_gz <- GZip.decompress <$> BL.readFile (dir </> "test.mps.gz")
    case nativeNewline of
      CRLF -> isCRLFByteString s_gz @?= True
      LF -> isLFByteString s_gz @?= True

    MIP.writeFile opt{ MIP.optNewline = Just LF } (dir </> "test_lf.mps.gz") prob
    prob_lf_gz <- MIP.readFile opt (dir </> "test_lf.mps.gz")
    prob_lf_gz @?= prob
    s_lf_gz <- GZip.decompress <$> BL.readFile (dir </> "test_lf.mps.gz")
    isLFByteString s_lf_gz @?= True

    MIP.writeFile opt{ MIP.optNewline = Just CRLF } (dir </> "test_crlf.mps.gz") prob
    prob_crlf_gz <- MIP.readFile opt (dir </> "test_crlf.mps.gz")
    prob_crlf_gz @?= prob
    s_crlf_gz <- GZip.decompress <$> BL.readFile (dir </> "test_crlf.mps.gz")
    isCRLFByteString s_crlf_gz @?= True
#endif

case_toLPString :: Assertion
case_toLPString = do
  let opt = MIP.def{ MIP.optFileEncoding = Just utf8 }
  prob <- MIP.readFile opt "samples/lp/test.lp"
  case MIP.toLPString MIP.def prob of
    Left err -> assertFailure ("toLPString failed: " ++ err)
    Right s -> isLFText s @?= True

case_toMPSString :: Assertion
case_toMPSString = do
  let opt = MIP.def{ MIP.optFileEncoding = Just utf8 }
  prob <- MIP.readFile opt "samples/lp/test.lp"
  case MIP.toMPSString MIP.def prob of
    Left err -> assertFailure ("toMPSString failed: " ++ err)
    Right s -> isLFText s @?= True

isLFText :: TL.Text -> Bool
isLFText s = not ('\r' `TL.elem` s)

-- isCRLFText :: TL.Text -> Bool
-- isCRLFText s = all (\(c1, c2) -> not (c2 == '\n') || c1 == '\r') $ TL.zip s (TL.tail s)

isLFByteString :: BL.ByteString -> Bool
isLFByteString s = '\r' `BL.notElem` s

isCRLFByteString :: BL.ByteString -> Bool
isCRLFByteString s = and $ BL.zipWith (\c1 c2 -> not (c2 == '\n') || c1 == '\r') s (BL.tail s)

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

case_MIPLIBSol :: Assertion
case_MIPLIBSol = do
  sol <- MIPLIBSol.readFile "samples/lp/test-solution-miplib.sol"
  sol @?=
    MIP.Solution
    { MIP.solStatus = MIP.StatusFeasible
    , MIP.solObjectiveValue = Just 122.5
    , MIP.solVariables = Map.fromList [("x1", 40), ("x2", 10.5), ("x3", 19.5), ("x4", 3)]
    }
  withSystemTempDirectory "MIP" $ \dir -> do
    MIPLIBSol.writeFile (dir </> "test.sol") sol
    sol2 <- MIPLIBSol.readFile (dir </> "test.sol")
    sol2 @?= sol

case_MIPLIBSol_infeasible :: Assertion
case_MIPLIBSol_infeasible = do
  sol <- MIPLIBSol.readFile "samples/lp/test-solution-miplib-infeasible.sol"
  sol @?=
    MIP.Solution
    { MIP.solStatus = MIP.StatusInfeasible
    , MIP.solObjectiveValue = Nothing
    , MIP.solVariables = Map.empty
    }
  withSystemTempDirectory "MIP" $ \dir -> do
    MIPLIBSol.writeFile (dir </> "test.sol") sol
    sol2 <- MIPLIBSol.readFile (dir </> "test.sol")
    sol2 @?= sol

mipTestGroup :: TestTree
mipTestGroup = $(testGroupGenerator)
