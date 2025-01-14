{-# LANGUAGE TemplateHaskell #-}
module Test.MPSFile (mpsTestGroup) where

import Control.Monad
import Data.Default.Class
import Data.Either
import Data.List
import Data.Maybe
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.TH
import Numeric.Optimization.MIP.MPSFile

case_testdata = checkString "testdata" testdata
case_example2 = checkFile "samples/mps/example2.mps"
case_ind1     = checkFile "samples/mps/ind1.mps"
case_intvar1  = checkFile "samples/mps/intvar1.mps"
case_intvar2  = checkFile "samples/mps/intvar2.mps"
case_quadobj1 = checkFile "samples/mps/quadobj1.mps"
case_quadobj2 = checkFile "samples/mps/quadobj2.mps"
case_ranges   = checkFile "samples/mps/ranges.mps"
case_sos      = checkFile "samples/mps/sos.mps"
case_sc       = checkFile "samples/mps/sc.mps"
case_semicont = checkFile "samples/mps/test-semicont.mps"
case_semiint  = checkFile "samples/mps/test-semiint.mps"

------------------------------------------------------------------------
-- Sample data

testdata :: String
testdata = unlines
  [ "NAME          example2.mps"
  , "ROWS"
  , " N  obj     "
  , " L  c1      "
  , " L  c2      "
  , "COLUMNS"
  , "    x1        obj                 -1   c1                  -1"
  , "    x1        c2                   1"
  , "    x2        obj                 -2   c1                   1"
  , "    x2        c2                  -3"
  , "    x3        obj                 -3   c1                   1"
  , "    x3        c2                   1"
  , "RHS"
  , "    rhs       c1                  20   c2                  30"
  , "BOUNDS"
  , " UP BOUND     x1                  40"
  , "ENDATA"
  ]

------------------------------------------------------------------------
-- Utilities

checkFile :: FilePath -> Assertion
checkFile fname = do
  lp <- parseFile def fname
  case render def lp of
    Left err -> assertFailure ("render failure: " ++ err)
    Right str -> assertBool ("failed to parse " ++ show str)  $ isRight $ parseString def fname str

checkString :: String -> String -> Assertion
checkString name str = do
  case parseString def name str of
    Left err -> assertFailure (show err)
    Right lp ->
      case render def lp of
        Left err -> assertFailure ("render failure: " ++ err)
        Right str -> assertBool ("failed to parse " ++ show str)  $ isRight $ parseString def name str

------------------------------------------------------------------------
-- Test harness

mpsTestGroup :: TestTree
mpsTestGroup = $(testGroupGenerator)
