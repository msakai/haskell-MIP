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
import qualified Data.Map as Map

data SolveOptions
  = SolveOptions
  { solveTimeLimit :: Maybe Double
    -- ^ time limit in seconds
  , solveLogger :: String -> IO ()
    -- ^ invoked when a solver output a line
  , solveErrorLogger :: String -> IO ()
    -- ^ invoked when a solver output a line to stderr
  , solveCondensedSolution :: Bool
    -- ^ potentially omit variables set to zero from the solution
  }

instance Default SolveOptions where
  def =
    SolveOptions
    { solveTimeLimit = Nothing
    , solveLogger = const $ return ()
    , solveErrorLogger = const $ return ()
    , solveCondensedSolution = False
    }


class Monad m => IsSolver s m | s -> m where
  solve' :: s -> SolveOptions -> MIP.Problem Scientific -> m (MIP.Solution Scientific)
  solve  :: s -> SolveOptions -> MIP.Problem Scientific -> m (MIP.Solution Scientific)
  solve s opts problem = (if solveCondensedSolution opts then id else addZeroes problem) <$> solve' s opts problem
  {-# MINIMAL solve' #-}

-- Several solvers (at least CBC) do not include any variables set to 0 in their solution.
-- TODO: for solvers that do return all variables, add `solve = solve'`
-- for a minor performance improvement.
addZeroes :: MIP.Problem Scientific -> MIP.Solution Scientific -> MIP.Solution Scientific
addZeroes problem (Solution stat obj solmap) = 
  -- Map.union is left-biased: only values not present in the solution are added.
  Solution stat obj $ Map.union solmap (Map.fromSet (const 0) (vars problem))
