{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE FunctionalDependencies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.Solver.Base
-- Copyright   :  (c) Masahiro Sakai 2017
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP.Solver.Base
  ( SolveOptions (..)
  , Default (..)
  , IsSolver (..)
  ) where

import Data.Default.Class
import Data.Scientific (Scientific)
import Numeric.Optimization.MIP.Base as MIP

data SolveOptions
  = SolveOptions
  { solveTimeLimit :: Maybe Double
    -- ^ time limit in seconds
  , solveLogger :: String -> IO ()
    -- ^ invoked when a solver output a line
  , solveErrorLogger :: String -> IO ()
    -- ^ invoked when a solver output a line to stderr
  }

instance Default SolveOptions where
  def =
    SolveOptions
    { solveTimeLimit = Nothing
    , solveLogger = const $ return ()
    , solveErrorLogger = const $ return ()
    }

class Monad m => IsSolver s m | s -> m where
  solve :: s -> SolveOptions -> MIP.Problem Scientific -> m (MIP.Solution Scientific)
