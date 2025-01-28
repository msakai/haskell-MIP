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
  (
  -- * Solver type
    IsSolver (..)
  , SolveOptions (..)
  -- * Utilities
  , Default (..)
  ) where

import Data.Default.Class
import Data.Scientific (Scientific)
import Numeric.Optimization.MIP.Base as MIP
import qualified Data.Map as Map

-- | Options for 'solve' function
data SolveOptions
  = SolveOptions
  { solveTimeLimit :: Maybe Double
    -- ^ time limit in seconds
  , solveTol :: Maybe (MIP.Tol Scientific)
    -- ^ tolerance
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
    , solveTol = Nothing
    , solveLogger = const $ return ()
    , solveErrorLogger = const $ return ()
    , solveCondensedSolution = False
    }


-- | Type class for solvers
--
-- 
class Monad m => IsSolver s m | s -> m where
  -- | Low level version of 'solve'' that allows omission of variables with a value 0.
  --
  -- Implementor of the type class must implement this method.
  solve' :: s -> SolveOptions -> MIP.Problem Scientific -> m (MIP.Solution Scientific)

  -- | A method for solving 'MIP.Problem'
  --
  -- This method is a bit higher level than 'solve'' in that it does not omit variables
  -- with a value @0@ unless 'solveCondensedSolution' is set to @True@.
  -- Implementor of the type class can override this method as @solve = solve'@ if the
  -- solver always returns all variables.
  solve  :: s -> SolveOptions -> MIP.Problem Scientific -> m (MIP.Solution Scientific)
  solve s opts problem = (if solveCondensedSolution opts then id else addZeroes problem) <$> solve' s opts problem
  {-# MINIMAL solve' #-}

-- Several solvers (at least CBC) do not include any variables set to 0 in their solution.
-- TODO: for solvers that do return all variables, add @solve = solve'@
-- for a minor performance improvement.
addZeroes :: MIP.Problem Scientific -> MIP.Solution Scientific -> MIP.Solution Scientific
addZeroes problem (Solution stat obj solmap) = 
  -- Map.union is left-biased: only values not present in the solution are added.
  Solution stat obj $ Map.union solmap (Map.fromSet (const 0) (vars problem))
