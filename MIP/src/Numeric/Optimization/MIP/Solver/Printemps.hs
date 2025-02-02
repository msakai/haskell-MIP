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

import qualified Data.Aeson as J
import Data.Default.Class
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
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

-- | A solver instance for calling @mps_solver.exe@ command from [PRINTEMPS](https://snowberryfield.github.io/printemps/).
--
-- It requires PRINTEMPS version 2.6.0 or later.
--
-- Use 'printemps' and record update syntax to modify its field.
data Printemps
  = Printemps
  { printempsPath :: String
  , printempsArgs :: [String]
  }

instance Default Printemps where
  def = printemps

-- | Default value of t'Printemps'
printemps :: Printemps
printemps = Printemps "mps_solver" []

instance IsSolver Printemps IO where
  solve' solver opt prob = do
    let prob' = prob
                { name =
                    case name prob of
                      Just s | not (T.null s) -> Just s
                      _ -> Just "problem"
                }

    let (orig_option_file, args') = removeOptionArgs (printempsArgs solver)
    orig_option <-
      case orig_option_file of
        Nothing -> return Map.empty
        Just fname -> do
          ret <- J.eitherDecodeFileStrict' fname
          case ret of
            Left err -> ioError $ userError err
            Right option -> return option
    orig_general <-
      case J.fromJSON (Map.findWithDefault (J.object []) "general" orig_option) of
        J.Error err -> ioError $ userError err
        J.Success val -> return val
    let general :: Map T.Text J.Value
        general =
          case solveTimeLimit opt of
            Nothing -> orig_general
            Just t -> Map.insert "time_max" (J.toJSON t) orig_general
        option :: Map T.Text J.Value
        option = Map.insert "general" (J.toJSON general) orig_option

    withSystemTempDirectory "printemps" $ \path ->
      case MPSFile.render def{ optMPSWriteObjName = False } prob' of
        Left err -> ioError $ userError err
        Right s -> do
          let problem_file = path </> "input.mps"
          TLIO.writeFile problem_file s
          let option_file = path </> "option.json"
          J.encodeFile option_file option

          let args = ["-p", option_file] ++ args' ++ [problem_file]
              onGetLine s = solveLogger opt s
              onGetErrorLine = solveErrorLogger opt
          exitcode <- runProcessWithOutputCallback (printempsPath solver) args (Just path) "" onGetLine onGetErrorLine
          if exitcode /= ExitSuccess then do
            return $ def{ solStatus = StatusUnknown }
          else do
            PrintempsSol.readFile (path </> "incumbent.json")

removeOptionArgs :: [String] -> (Maybe FilePath, [String])
removeOptionArgs = f Nothing []
  where
    f optionFile args [] = (optionFile, reverse args)
    f _ args ("-p" : fname : xs) = f (Just fname) args xs
    f optionFile args (x : xs) = f optionFile (x : args) xs
