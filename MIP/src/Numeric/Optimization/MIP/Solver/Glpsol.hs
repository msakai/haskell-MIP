{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.Solver.Glpsol
-- Copyright   :  (c) Masahiro Sakai 2017
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP.Solver.Glpsol
  ( Glpsol (..)
  , glpsol
  ) where

import Algebra.PartialOrd
import Data.Default.Class
import Data.IORef
import qualified Data.Text.Lazy.IO as TLIO
import System.Exit
import System.IO
import System.IO.Temp
import qualified Numeric.Optimization.MIP.Base as MIP
import qualified Numeric.Optimization.MIP.LPFile as LPFile
import Numeric.Optimization.MIP.Solver.Base
import qualified Numeric.Optimization.MIP.Solution.GLPK as GLPKSol
import Numeric.Optimization.MIP.Internal.ProcessUtil (runProcessWithOutputCallback)

data Glpsol
  = Glpsol
  { glpsolPath :: String
  , glpsolArgs :: [String]
  }

instance Default Glpsol where
  def = glpsol

glpsol :: Glpsol
glpsol = Glpsol "glpsol" []

instance IsSolver Glpsol IO where
  solve' solver opt prob = do
    case LPFile.render def prob of
      Left err -> ioError $ userError err
      Right lp -> do
        withSystemTempFile "glpsol.lp" $ \fname1 h1 -> do
          TLIO.hPutStr h1 lp
          hClose h1
          withSystemTempFile "glpsol.sol" $ \fname2 h2 -> do
            hClose h2
            isUnboundedRef <- newIORef False
            isInfeasibleRef <- newIORef False
            let args = glpsolArgs solver ++
                       ["--lp", fname1, "-o", fname2] ++
                       (case solveTimeLimit opt of
                          Nothing -> []
                          Just sec -> ["--tmlim", show (max 1 (floor sec) :: Int)])
                onGetLine s = do
                  case s of
                    "LP HAS UNBOUNDED PRIMAL SOLUTION" ->
                      writeIORef isUnboundedRef True
                    "PROBLEM HAS UNBOUNDED SOLUTION" ->
                      writeIORef isUnboundedRef True
                    "PROBLEM HAS NO PRIMAL FEASIBLE SOLUTION" ->
                      writeIORef isInfeasibleRef True
                    _ -> return ()
                  solveLogger opt s
                onGetErrorLine = solveErrorLogger opt
            exitcode <- runProcessWithOutputCallback (glpsolPath solver) args Nothing "" onGetLine onGetErrorLine
            case exitcode of
              ExitFailure n -> ioError $ userError $ "exit with " ++ show n
              ExitSuccess -> do
                sol <- GLPKSol.readFile fname2
                isUnbounded <- readIORef isUnboundedRef
                isInfeasible <- readIORef isInfeasibleRef
                if isUnbounded && MIP.solStatus sol `leq` MIP.StatusInfeasibleOrUnbounded then
                  return $ sol{ MIP.solStatus = MIP.StatusInfeasibleOrUnbounded }
                else if isInfeasible && MIP.solStatus sol `leq` MIP.StatusInfeasible then
                  return $ sol{ MIP.solStatus = MIP.StatusInfeasible }
                else
                  return sol
