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

import Control.Monad
import Data.Default.Class
import qualified Data.Text.Lazy.IO as TLIO
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp
import qualified Numeric.Optimization.MIP.Base as MIP
import qualified Numeric.Optimization.MIP.LPFile as LPFile
import Numeric.Optimization.MIP.Solver.Base
import qualified Numeric.Optimization.MIP.Solution.CBC as CBCSol
import Numeric.Optimization.MIP.Internal.ProcessUtil (runProcessWithOutputCallback)

-- | A solver instance for calling @cbc@ command from [CBC (COIN-OR Branch-and-Cut solver)](https://github.com/coin-or/Cbc).
--
-- Use 'cbc' and record update syntax to modify its field.
data CBC
  = CBC
  { cbcPath :: String
  , cbcArgs :: [String]
  }

instance Default CBC where
  def = cbc

-- | Default value of t'CBC'
cbc :: CBC
cbc = CBC "cbc" []

instance IsSolver CBC IO where
  solve' solver opt prob = do
    case LPFile.render def prob{ MIP.objectiveFunction = obj' } of
      Left err -> ioError $ userError err
      Right lp -> do
        withSystemTempDirectory "haskell-mip-cbc" $ \dir -> do
          let fname1 = dir </> "cbc.lp"
              fname2 = dir </> "cbc.sol"
          TLIO.writeFile fname1 lp
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
              m <- doesFileExist fname2
              unless m $ ioError $ userError "CBC returned exit code 0, but wrote no solution file. You may want to use the solveLogger or solveErrorLogger for more information"
              sol <- CBCSol.readFile fname2
              if isMax then
                return $ sol{ MIP.solObjectiveValue = fmap negate (MIP.solObjectiveValue sol) }
              else
                return sol
    where
      obj = MIP.objectiveFunction prob
      isMax = MIP.objDir obj == MIP.OptMax
      obj' = if isMax then obj{ MIP.objDir = MIP.OptMin, MIP.objExpr = - MIP.objExpr obj } else obj
