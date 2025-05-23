{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP
-- Copyright   :  (c) Masahiro Sakai 2011-2014
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Mixed-Integer Programming Problems with some commmonly used extensions
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP
  (
  -- * Mixed-Integer Programming (MIP) problem specification

  -- ** MIP problems
    Problem (..)

  -- *** Set of variables
  , variables
  , continuousVariables
  , integerVariables
  , binaryVariables
  , semiContinuousVariables
  , semiIntegerVariables

  -- *** Variable's attributes
  , varTypes
  , varType
  , getVarType
  , varBounds
  , getBounds

  -- ** Variables
  , Var (Var)
  , varName
  , toVar
  , fromVar

  -- *** Variable domain
  , Domain

  -- *** Variable types
  , VarType (..)

  -- *** Variable bounds
  , BoundExpr
  , Extended (..)
  , Bounds
  , defaultBounds
  , defaultLB
  , defaultUB

  -- ** Labels
  , Label

  -- ** Expressions
  , Expr (Expr)
  , varExpr
  , constExpr
  , terms
  , Term (..)

  -- ** Objective function
  , OptDir (..)
  , ObjectiveFunction (..)

  -- ** Constraints

  -- *** Linear (or Quadratic or Polynomial) constraints
  , Constraint (..)
  , constrBounds
  , (.==.)
  , (.<=.)
  , (.>=.)
  , RelOp (..)

  -- *** SOS constraints
  , SOSType (..)
  , SOSConstraint (..)

  -- * Solutions
  , Solution (..)
  , Status (..)
  , meetStatus

  -- * Evaluation
  , Tol (..)
  , zeroTol
  , Eval (..)
  , isInDomain
  , isIntegral
  , isInBounds

  -- * File I/O
  -- $IO
  , FileOptions (..)
  , WriteSetting (..)
  , ParseError
  , isGZipSupported

  -- ** Reading problem files
  , readFile
  , readLPFile
  , readMPSFile
  , parseLPString
  , parseMPSString

  -- ** Generating problem files
  , writeFile
  , writeLPFile
  , writeMPSFile
  , toLPString
  , toMPSString

  -- * Utilities
  , Default (..)
  , Variables (..)
  , intersectBounds
  ) where

import Prelude hiding (readFile, writeFile)
import Control.Applicative
import Control.Exception
import Data.Scientific (Scientific)
import Data.String
import qualified Data.Text.Lazy as TL
import System.IO hiding (readFile, writeFile)
import Text.Megaparsec (Stream (..))

import Numeric.Optimization.MIP.Base
import Numeric.Optimization.MIP.FileUtils
import qualified Numeric.Optimization.MIP.LPFile as LPFile
import qualified Numeric.Optimization.MIP.MPSFile as MPSFile

-- | Parse LP or MPS file based on file extension.
readFile :: FileOptions -> FilePath -> IO (Problem Scientific)
readFile opt fname =
  case getBaseExtension fname of
    ".lp"  -> readLPFile opt fname
    ".mps" -> readMPSFile opt fname
    ext -> ioError $ userError $ "unknown extension: " ++ ext

-- | Parse a file containing LP file data.
readLPFile :: FileOptions -> FilePath -> IO (Problem Scientific)
readLPFile = LPFile.parseFile

-- | Parse a file containing MPS file data.
readMPSFile :: FileOptions -> FilePath -> IO (Problem Scientific)
readMPSFile = MPSFile.parseFile

-- | Parse a string containing LP file data.
parseLPString :: (Stream s, Token s ~ Char, IsString (Tokens s)) => FileOptions -> String -> s -> Either (ParseError s) (Problem Scientific)
parseLPString = LPFile.parseString

-- | Parse a string containing MPS file data.
parseMPSString :: (Stream s, Token s ~ Char, IsString (Tokens s)) => FileOptions -> String -> s -> Either (ParseError s) (Problem Scientific)
parseMPSString = MPSFile.parseString

-- | Generate LP file or MPS file based on file extension.
writeFile :: FileOptions -> FilePath -> Problem Scientific -> IO ()
writeFile opt fname prob =
  case getBaseExtension fname of
    ".lp"  -> writeLPFile opt fname prob
    ".mps" -> writeMPSFile opt fname prob
    ext -> ioError $ userError $ "unknown extension: " ++ ext

-- | Generate LP file.
writeLPFile :: FileOptions -> FilePath -> Problem Scientific -> IO ()
writeLPFile opt fname prob =
  case LPFile.render opt{ optNewline = optNewline opt <|> Just nativeNewline } prob of
    Left err -> ioError $ userError err
    Right s -> writeTextFile opt fname (isAscii prob) s

-- | Generate MPS file.
writeMPSFile :: FileOptions -> FilePath -> Problem Scientific -> IO ()
writeMPSFile opt fname prob =
  case MPSFile.render opt{ optNewline = optNewline opt <|> Just nativeNewline } prob of
    Left err -> ioError $ userError err
    Right s -> writeTextFile opt fname (isAscii prob) s

-- | Generate a 'TL.Text' containing LP file data.
toLPString :: FileOptions -> Problem Scientific -> Either String TL.Text
toLPString opt = LPFile.render opt{ optNewline = optNewline opt <|> Just LF }

-- | Generate a 'TL.Text' containing MPS file data.
toMPSString :: FileOptions -> Problem Scientific -> Either String TL.Text
toMPSString opt = MPSFile.render opt{ optNewline = optNewline opt <|> Just LF }

-- $IO
-- If this library is built with @WithZlib@ flag (enabled by default), 
-- reading/writing gzipped file (@.gz@) are also supported.
-- Availability of gzipped file support can ben checked using 'isGZipSupported'.
