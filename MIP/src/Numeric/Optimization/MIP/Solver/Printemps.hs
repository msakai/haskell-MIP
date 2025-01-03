{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.Solver.Printemps
-- Copyright   :  (c) Masahiro Sakai 2025
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP.Solver.Printemps
  ( Printemps (..)
  , printemps
  ) where

import Data.Default.Class
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO
import System.IO.Temp
import qualified Numeric.Optimization.MIP.MPSFile as MPSFile
import Numeric.Optimization.MIP.Base
import Numeric.Optimization.MIP.Solver.Base
import qualified Numeric.Optimization.MIP.Solution.Printemps as PrintempsSol
import Numeric.Optimization.MIP.Internal.ProcessUtil (runProcessWithOutputCallback)
import System.Exit
import System.FilePath ((</>))

data Printemps
  = Printemps
  { printempsPath :: String
  , printempsArgs :: [String]
  }

instance Default Printemps where
  def = printemps

printemps :: Printemps
printemps = Printemps "mps_solver.exe" []

instance IsSolver Printemps IO where
  solve' solver opt prob = do
    let prob' = prob
                { name =
                    case name prob of
                      Just s | not (T.null s) -> Just s
                      _ -> Just "problem"
                }
        obj = objectiveFunction prob'
        (prob'', postProcess) =
          case objDir obj of
            OptMin -> (prob', id)
            OptMax -> (prob'{ objectiveFunction = obj{ objDir = OptMin, objExpr = negate (objExpr obj) } }, negate)

    withSystemTempDirectory "printemps" $ \path ->
      case MPSFile.render def{ optMPSWriteObjSense = WriteIfNotDefault } prob'' of
        Left err -> ioError $ userError err
        Right s -> do
          let fname1 = path </> "input.mps"
          TLIO.writeFile fname1 s
          let args = printempsArgs solver ++ [fname1]
              onGetLine s = solveLogger opt s
              onGetErrorLine = solveErrorLogger opt
          exitcode <- runProcessWithOutputCallback (printempsPath solver) args (Just path) "" onGetLine onGetErrorLine
          if exitcode /= ExitSuccess then do
            return $ def{ solStatus = StatusUnknown }
          else do
            sol <- PrintempsSol.readFile (path </> "incumbent.json")
            return $ sol{ solObjectiveValue = fmap postProcess (solObjectiveValue sol) }
