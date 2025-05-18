{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.Solution.Printemps
-- Copyright   :  (c) Masahiro Sakai 2025
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP.Solution.Printemps
  ( Solution (..)
  , readFile
  ) where

import Prelude hiding (readFile, writeFile)

import qualified Data.Aeson as J
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Scientific (Scientific)
import qualified Data.Text as T
import GHC.Generics

import qualified Numeric.Optimization.MIP.Base as MIP
import Numeric.Optimization.MIP.Base (Solution (..))

data Incumbent
  = Incumbent
  { incumbentVersion :: T.Text
  , incumbentName :: T.Text
  , incumbentNumberOfVariables :: Int
  , incumbentIsFoundFeasibleSolution :: Bool
  , incumbentObjective :: Scientific
  , incumbentTotalViolation :: Scientific
  , incumbentVariables :: Map T.Text Scientific
  , incumbentExpressions :: Map T.Text Scientific
  , incumbentConstraints :: Map T.Text Scientific
  , incumbentViolations :: Map T.Text Scientific
  }
  deriving (Generic, Show)

customOptions :: J.Options
customOptions =
  J.defaultOptions
  { J.fieldLabelModifier = J.camelTo2 '_' . drop (length "Incumbent")
  }

instance J.FromJSON Incumbent where
  parseJSON = J.genericParseJSON customOptions

readFile :: FilePath -> IO (MIP.Solution Scientific)
readFile fname = do
  ret <- J.eitherDecodeFileStrict' fname
  case ret of
    Left err -> ioError $ userError err
    Right incumbent -> return $
      MIP.Solution
      { MIP.solStatus =
          if incumbentIsFoundFeasibleSolution incumbent
          then MIP.StatusFeasible
          else MIP.StatusUnknown
      , MIP.solObjectiveValue =
          Just (incumbentObjective incumbent)
      , MIP.solVariables =
          Map.fromList [(MIP.Var v, val) | (v, val) <- Map.toList (incumbentVariables incumbent)]
      }
