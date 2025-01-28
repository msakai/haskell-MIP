{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.Solver.CPLEX
-- Copyright   :  (c) Masahiro Sakai 2017
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP.Solver.CPLEX
  ( CPLEX (..)
  , cplex
  ) where

import Control.Monad
import Data.Default.Class
import Data.IORef
import qualified Data.Text.Lazy.IO as TLIO
import System.Exit
import System.IO
import System.IO.Temp
import qualified Numeric.Optimization.MIP.Base as MIP
import qualified Numeric.Optimization.MIP.LPFile as LPFile
import Numeric.Optimization.MIP.Solver.Base
import qualified Numeric.Optimization.MIP.Solution.CPLEX as CPLEXSol
import Numeric.Optimization.MIP.Internal.ProcessUtil (runProcessWithOutputCallback)

data CPLEX
  = CPLEX
  { cplexPath :: String
  , cplexArgs :: [String]                 
  , cplexCommands :: [String]
  }

instance Default CPLEX where
  def = cplex

cplex :: CPLEX
cplex = CPLEX "cplex" [] []

instance IsSolver CPLEX IO where
  solve' solver opt prob = do
    case LPFile.render def prob of
      Left err -> ioError $ userError err
      Right lp -> do
        withSystemTempFile "cplex.lp" $ \fname1 h1 -> do
          TLIO.hPutStr h1 lp
          hClose h1
          withSystemTempFile "cplex.sol" $ \fname2 h2 -> do
            hClose h2
            isInfeasibleRef <- newIORef False
            let input = unlines $
                  (case solveTimeLimit opt of
                     Nothing -> []
                     Just sec -> ["set timelimit " ++ show sec]) ++
                  (case solveTol opt of
                     Nothing -> []
                     Just tol ->
                       [ "set mip tolerances integrality " ++ show (MIP.integralityTol tol)
                       , "set simplex tolerances feasibility " ++ show (MIP.feasibilityTol tol)
                       , "set simplex tolerances optimality " ++ show (MIP.optimalityTol tol)
                       ]
                  ) ++
                  [ "read " ++ show fname1 ] ++
                  cplexCommands solver ++
                  [ "optimize"
                  , "write " ++ show fname2
                  , "y"
                  , "quit"
                  ]
                onGetLine s = do
                  when (s == "MIP - Integer infeasible.") $ do
                    writeIORef isInfeasibleRef True
                  solveLogger opt s
                onGetErrorLine = solveErrorLogger opt
            exitcode <- runProcessWithOutputCallback (cplexPath solver) (cplexArgs solver) Nothing input onGetLine onGetErrorLine
            case exitcode of
              ExitFailure n -> ioError $ userError $ "exit with " ++ show n
              ExitSuccess -> do
                size <- withFile fname2 ReadMode $ hFileSize
                if size == 0 then do
                  isInfeasible <- readIORef isInfeasibleRef
                  if isInfeasible then
                    return def{ MIP.solStatus = MIP.StatusInfeasible }
                  else
                    return def
                else
                  CPLEXSol.readFile fname2
