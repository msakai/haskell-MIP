{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.Solver.LPSolve
-- Copyright   :  (c) Masahiro Sakai 2017
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP.Solver.LPSolve
  ( LPSolve (..)
  , lpSolve
  ) where

import Control.Monad
import Data.Default.Class
import Data.IORef
import Data.List (stripPrefix)
import qualified Data.Map as Map
import Data.String
import qualified Data.Text.Lazy.IO as TLIO
import System.Exit
import System.IO
import System.IO.Temp
import qualified Numeric.Optimization.MIP.Base as MIP
import qualified Numeric.Optimization.MIP.MPSFile as MPSFile
import Numeric.Optimization.MIP.Solver.Base
import Numeric.Optimization.MIP.Internal.ProcessUtil (runProcessWithOutputCallback)

-- | A solver instance for calling @lp_solve@ command from [lp_solve](https://lpsolve.sourceforge.net/5.5/) package.
--
-- Use 'lpSolve' and record update syntax to modify its field.
data LPSolve
  = LPSolve
  { lpSolvePath :: String
  , lpSolveArgs :: [String]
  }

instance Default LPSolve where
  def = lpSolve

-- | Default value of t'LPSolve'
lpSolve :: LPSolve
lpSolve = LPSolve "lp_solve" []

instance IsSolver LPSolve IO where
  solve' solver opt prob = do
    case MPSFile.render def prob of
      Left err -> ioError $ userError err
      Right lp -> do
        withSystemTempFile "lp_solve.mps" $ \fname1 h1 -> do
          TLIO.hPutStr h1 lp
          hClose h1
          objRef <- newIORef Nothing
          solRef <- newIORef []
          flagRef <- newIORef False
          let args = lpSolveArgs solver
                  ++ (case solveTimeLimit opt of
                        Nothing -> []
                        Just sec -> ["-timeout", show sec])
                  ++ (case solveTol opt of
                        Nothing -> []
                        Just tol ->
                          [ "-e", show (MIP.integralityTol tol)
                          , "-epsb", show (MIP.feasibilityTol tol)
                          , "-epsd", show (MIP.optimalityTol tol)
                          ])
                  ++ ["-fmps", fname1]
              onGetLine s = do
                case s of
                  "Actual values of the variables:" -> writeIORef flagRef True
                  _ | Just v <- stripPrefix "Value of objective function: " s -> do
                    writeIORef objRef (Just (read v))
                  _ -> do
                    flag <- readIORef flagRef
                    when flag $ do
                      case words s of
                        [var,val] -> modifyIORef solRef ((fromString var, read val) :)
                        _ -> return ()
                    return ()
                solveLogger opt s
              onGetErrorLine = solveErrorLogger opt
          exitcode <- runProcessWithOutputCallback (lpSolvePath solver) args Nothing "" onGetLine onGetErrorLine
          status <-
            case exitcode of
              ExitSuccess      -> return MIP.StatusOptimal
              ExitFailure (-2) -> return MIP.StatusUnknown               -- NOMEMORY
              ExitFailure 1    -> return MIP.StatusFeasible              -- SUBOPTIMAL
              ExitFailure 2    -> return MIP.StatusInfeasible            -- INFEASIBLE
              ExitFailure 3    -> return MIP.StatusInfeasibleOrUnbounded -- UNBOUNDED
              ExitFailure 4    -> return MIP.StatusUnknown               -- DEGENERATE
              ExitFailure 5    -> return MIP.StatusUnknown               -- NUMFAILURE
              ExitFailure 6    -> return MIP.StatusUnknown               -- USERABORT
              ExitFailure 7    -> return MIP.StatusUnknown               -- TIMEOUT
              ExitFailure 9    -> return MIP.StatusOptimal               -- PRESOLVED
              ExitFailure 25   -> return MIP.StatusUnknown               -- NUMFAILURE
              ExitFailure n    -> ioError $ userError $ "unknown exit code: " ++ show n
          obj <- readIORef objRef
          sol <- readIORef solRef
          return $
            MIP.Solution
            { MIP.solStatus = status
            , MIP.solObjectiveValue = obj
            , MIP.solVariables = Map.fromList sol
            }
