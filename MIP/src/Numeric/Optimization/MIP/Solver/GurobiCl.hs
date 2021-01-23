{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.Solver.GurobiCl
-- Copyright   :  (c) Masahiro Sakai 2017
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP.Solver.GurobiCl
  ( GurobiCl (..)
  , gurobiCl
  ) where

import Data.Default.Class
import Data.IORef
import Data.List (isPrefixOf)
import qualified Data.Text.Lazy.IO as TLIO
import System.Exit
import System.IO
import System.IO.Temp
import qualified Numeric.Optimization.MIP.Base as MIP
import qualified Numeric.Optimization.MIP.LPFile as LPFile
import Numeric.Optimization.MIP.Solver.Base
import qualified Numeric.Optimization.MIP.Solution.Gurobi as GurobiSol
import Numeric.Optimization.MIP.Internal.ProcessUtil (runProcessWithOutputCallback)

data GurobiCl
  = GurobiCl
  { gurobiClPath :: String
  , gurobiClArgs :: [String]
  }

instance Default GurobiCl where
  def = gurobiCl

gurobiCl :: GurobiCl
gurobiCl = GurobiCl "gurobi_cl" []

instance IsSolver GurobiCl IO where
  solve solver opt prob = do
    case LPFile.render def prob of
      Left err -> ioError $ userError err
      Right lp -> do
        withSystemTempFile "gurobi.lp" $ \fname1 h1 -> do
          TLIO.hPutStr h1 lp
          hClose h1
          withSystemTempFile "gurobi.sol" $ \fname2 h2 -> do
            hClose h2
            statusRef <- newIORef MIP.StatusUnknown
            let args = gurobiClArgs solver
                    ++ ["ResultFile=" ++ fname2]
                    ++ (case solveTimeLimit opt of
                          Nothing -> []
                          Just sec -> ["TimeLimit=" ++ show sec])
                    ++ [fname1]
                onGetLine s = do
                  case s of
                    -- "Time limit reached" -> writeIORef statusRef MIP.StatusUnknown
                    "Model is unbounded" -> writeIORef statusRef MIP.StatusUnbounded
                    "Model is infeasible" -> writeIORef statusRef MIP.StatusInfeasible
                    "Model is infeasible or unbounded" -> writeIORef statusRef MIP.StatusInfeasibleOrUnbounded
                    _ | isPrefixOf "Optimal solution found" s -> writeIORef statusRef MIP.StatusOptimal
                    _ -> return ()
                  solveLogger opt s
                onGetErrorLine = solveErrorLogger opt
            exitcode <- runProcessWithOutputCallback (gurobiClPath solver) args "" onGetLine onGetErrorLine
            case exitcode of
              ExitFailure n -> ioError $ userError $ "exit with " ++ show n
              ExitSuccess -> do
                status <- readIORef statusRef
                sol <- GurobiSol.readFile fname2
                return $ sol{ MIP.solStatus = status }
