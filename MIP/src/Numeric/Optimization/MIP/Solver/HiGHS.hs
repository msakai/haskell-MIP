{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.Solver.HiGHS
-- Copyright   :  (c) Masahiro Sakai 2025
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP.Solver.HiGHS
  ( HiGHS (..)
  , highs
  ) where

import Data.Default.Class
import qualified Data.Text.Lazy.IO as TLIO
import System.IO
import System.IO.Temp
import qualified Numeric.Optimization.MIP.LPFile as LPFile
import Numeric.Optimization.MIP.Solver.Base
import qualified Numeric.Optimization.MIP.Solution.HiGHS as HiGHSSol
import Numeric.Optimization.MIP.Internal.ProcessUtil (runProcessWithOutputCallback)

data HiGHS
  = HiGHS
  { highsPath :: String
  , highsArgs :: [String]
  }

instance Default HiGHS where
  def = highs

highs :: HiGHS
highs = HiGHS "highs" []

instance IsSolver HiGHS IO where
  solve' solver opt prob = do
    case LPFile.render def prob of
      Left err -> ioError $ userError err
      Right lp -> do
        withSystemTempFile "highs.lp" $ \fname1 h1 -> do
          TLIO.hPutStr h1 lp
          hClose h1
          withSystemTempFile "highs.sol" $ \fname2 h2 -> do
            hClose h2
            let args = highsArgs solver ++
                       ["--model_file", fname1, "--solution_file", fname2] ++
                       (case solveTimeLimit opt of
                          Nothing -> []
                          Just sec -> ["--time_limit", show sec])
                onGetLine s = solveLogger opt s
                onGetErrorLine = solveErrorLogger opt
            _exitcode <- runProcessWithOutputCallback (highsPath solver) args "" onGetLine onGetErrorLine
            HiGHSSol.readFile fname2
