{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.Solver.SCIP
-- Copyright   :  (c) Masahiro Sakai 2017
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP.Solver.SCIP
  ( SCIP (..)
  , scip
  ) where

import Data.Default.Class
import qualified Data.Text.Lazy.IO as TLIO
import System.Exit
import System.IO
import System.IO.Temp
import qualified Numeric.Optimization.MIP.LPFile as LPFile
import qualified Numeric.Optimization.MIP.Base as MIP
import Numeric.Optimization.MIP.Solver.Base
import qualified Numeric.Optimization.MIP.Solution.SCIP as ScipSol
import Numeric.Optimization.MIP.Internal.ProcessUtil (runProcessWithOutputCallback)

-- | A solver instance for calling @scip@ command from [SCIP Optimization Suite](https://www.scipopt.org/).
--
-- Use 'scip' and record update syntax to modify its field.
data SCIP
  = SCIP
  { scipPath :: String
  , scipArgs :: [String]
  , scipCommands :: [String]
  }

instance Default SCIP where
  def = scip

-- | Default value of t'SCIP'
scip :: SCIP
scip = SCIP "scip" [] []

instance IsSolver SCIP IO where
  solve' solver opt prob = do
    case LPFile.render def prob of
      Left err -> ioError $ userError err
      Right lp -> do
        withSystemTempFile "scip.lp" $ \fname1 h1 -> do
          TLIO.hPutStr h1 lp
          hClose h1
          withSystemTempFile "scip.sol" $ \fname2 h2 -> do
            hClose h2
            let commands = 
                  [ "read " ++ show fname1 ] ++
                  (case solveTimeLimit opt of
                     Nothing -> []
                     Just sec -> ["set limits time " ++ show sec]) ++
                  (case solveTol opt of
                     Nothing -> []
                     Just tol ->
                       [ "set numeric feastol " ++ show (MIP.feasibilityTol tol)
                       , "set numeric dualfeastol " ++ show (MIP.optimalityTol tol)
                       , -- https://listserv.zib.de/pipermail/scip/2010-September/000552.html
                         "set numeric epsilon " ++ show (MIP.integralityTol tol)
                       ]
                  ) ++
                  scipCommands solver ++
                  [ "optimize"
                  , "write solution " ++ show fname2
                  , "quit"
                  ]
                args = scipArgs solver ++ concat [["-c", cmd] | cmd <- commands]
                onGetLine = solveLogger opt
                onGetErrorLine = solveErrorLogger opt
            exitcode <- runProcessWithOutputCallback (scipPath solver) args Nothing "" onGetLine onGetErrorLine
            case exitcode of
              ExitFailure n -> ioError $ userError $ "exit with " ++ show n
              ExitSuccess -> ScipSol.readFile fname2
