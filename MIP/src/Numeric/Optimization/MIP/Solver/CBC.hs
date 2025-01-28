{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.Solver.CBC
-- Copyright   :  (c) Masahiro Sakai 2017
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP.Solver.CBC
  ( CBC (..)
  , cbc
  ) where

import Data.Default.Class
import qualified Data.Text.Lazy.IO as TLIO
import System.Exit
import System.IO
import System.IO.Temp
import qualified Numeric.Optimization.MIP.Base as MIP
import qualified Numeric.Optimization.MIP.LPFile as LPFile
import Numeric.Optimization.MIP.Solver.Base
import qualified Numeric.Optimization.MIP.Solution.CBC as CBCSol
import Numeric.Optimization.MIP.Internal.ProcessUtil (runProcessWithOutputCallback)

data CBC
  = CBC
  { cbcPath :: String
  , cbcArgs :: [String]
  }

instance Default CBC where
  def = cbc

cbc :: CBC
cbc = CBC "cbc" []

instance IsSolver CBC IO where
  solve' solver opt prob = do
    case LPFile.render def prob{ MIP.objectiveFunction = obj' } of
      Left err -> ioError $ userError err
      Right lp -> do
        withSystemTempFile "cbc.lp" $ \fname1 h1 -> do
          TLIO.hPutStr h1 lp
          hClose h1
          withSystemTempFile "cbc.sol" $ \fname2 h2 -> do
            hClose h2
            let args = cbcArgs solver
                    ++ [fname1]
                    ++ (case solveTimeLimit opt of
                          Nothing -> []
                          Just sec -> ["sec", show sec])
                    ++ (case solveTol opt of
                          Nothing -> []
                          Just tol ->
                            [ "integerTolerance", show (MIP.integralityTol tol)
                            , "primalTolerance", show (MIP.feasibilityTol tol)
                            , "dualTolerance", show (MIP.optimalityTol tol)
                            ])
                    ++ ["solve", "solu", fname2]
                onGetLine = solveLogger opt
                onGetErrorLine = solveErrorLogger opt
            exitcode <- runProcessWithOutputCallback (cbcPath solver) args Nothing "" onGetLine onGetErrorLine
            case exitcode of
              ExitFailure n -> ioError $ userError $ "exit with " ++ show n
              ExitSuccess -> do
                sol <- CBCSol.readFile fname2
                if isMax then
                  return $ sol{ MIP.solObjectiveValue = fmap negate (MIP.solObjectiveValue sol) }
                else
                  return sol
    where
      obj = MIP.objectiveFunction prob
      isMax = MIP.objDir obj == MIP.OptMax
      obj' = if isMax then obj{ MIP.objDir = MIP.OptMin, MIP.objExpr = - MIP.objExpr obj } else obj
