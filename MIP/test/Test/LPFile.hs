{-# LANGUAGE TemplateHaskell #-}
module Test.LPFile (lpTestGroup) where

import Control.Monad
import Data.Default.Class
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as TL
import System.IO
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.TH
import Numeric.Optimization.MIP.Base (FileOptions (..))
import Numeric.Optimization.MIP.LPFile

case_testdata       = checkString "testdata" testdata
case_test_bounds_free = checkFile "samples/lp/test-bounds-free.lp"
case_test_bounds_fixed = checkFile "samples/lp/test-bounds-fixed.lp"
case_test_indicator = checkFile "samples/lp/test-indicator.lp"
case_test_problem_name = checkFile "samples/lp/test-problem-name.lp"
case_test_qcp       = checkFile "samples/lp/test-qcp.lp"
case_test_qcp2      = checkFile "samples/lp/test-qcp2.lp"
case_test_qp        = checkFile "samples/lp/test-qp.lp"
case_test_semicont  = checkFile "samples/lp/test-semicont.lp"
case_test_semiint   = checkFile "samples/lp/test-semiint.lp"
case_test_sos       = checkFile "samples/lp/test-sos.lp"
case_test_sos2      = checkFile "samples/lp/test-sos2.lp"
case_test_lazy_constraints = checkFile "samples/lp/test-lazy-constraints.lp"
case_test_user_cuts = checkFile "samples/lp/test-user-cuts.lp"
case_empty_obj_1    = checkFile "samples/lp/empty_obj_1.lp"
case_empty_obj_2    = checkFile "samples/lp/empty_obj_2.lp"

case_render_newline :: Assertion
case_render_newline = do
  let Right prob = parseString def "testdata" testdata
  case render def{ optNewline = Just LF } prob of
    Left err -> assertFailure ("render failure: " ++ err)
    Right s -> do
      parseString def "testdata2" s @?= Right prob
      isLFText s @?= True
  case render def{ optNewline = Just CRLF } prob of
    Left err -> assertFailure ("render failure: " ++ err)
    Right s -> do
      parseString def "testdata2" s @?= Right prob
      isCRLFText s @?= True

------------------------------------------------------------------------
-- Sample data

testdata :: String
testdata = unlines
  [ "Maximize"
  , " obj: x1 + 2 x2 + 3 x3 + x4"
  , "Subject To"
  , " c1: - x1 + x2 + x3 + 10 x4 <= 20"
  , " c2: x1 - 3 x2 + x3 <= 30"
  , " c3: x2 - 3.5 x4 = 0"
  , "Bounds"
  , " 0 <= x1 <= 40"
  , " 2 <= x4 <= 3"
  , "General"
  , " x4"
  , "End"
  ]

------------------------------------------------------------------------
-- Utilities

checkFile :: FilePath -> Assertion
checkFile fname = do
  lp <- parseFile def fname
  case render def lp of
    Left err -> assertFailure ("render failure: " ++ err)
    Right str -> -- parseString def fname str @?= Right lp
      case parseString def fname str of
        Left err -> print str >> undefined
        Right _ -> return ()

checkString :: String -> String -> Assertion
checkString name str = do
  case parseString def name str of
    Left err -> assertFailure $ show err
    Right lp ->
      case render def lp of
        Left err -> assertFailure ("render failure: " ++ err)
        Right str -> parseString def name str @?= Right lp

isLFText :: TL.Text -> Bool
isLFText s = not $ TL.any ('\r' ==) s

isCRLFText :: TL.Text -> Bool
isCRLFText s = all (\(c1, c2) -> not (c2 == '\n') || c1 == '\r') $ TL.zip s (TL.tail s)

------------------------------------------------------------------------
-- Test harness

lpTestGroup :: TestTree
lpTestGroup = $(testGroupGenerator)
