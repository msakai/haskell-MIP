{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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

  -- ** Reading problem files
  , readFile
  , readLPFile
  , readMPSFile
  , parseLPString
  , parseMPSString
  , ParseError

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
import Data.Char
import Data.Scientific (Scientific)
import Data.String
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import System.FilePath (takeExtension, splitExtension)
import System.IO hiding (readFile, writeFile)
import Text.Megaparsec (Stream (..))

import Numeric.Optimization.MIP.Base
import Numeric.Optimization.MIP.FileUtils (ParseError)
import qualified Numeric.Optimization.MIP.LPFile as LPFile
import qualified Numeric.Optimization.MIP.MPSFile as MPSFile

#ifdef WITH_ZLIB
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Encoding (encode, decode)
import qualified Data.CaseInsensitive as CI
import GHC.IO.Encoding (getLocaleEncoding)
#endif

-- | Parse LP or MPS file based on file extension.
readFile :: FileOptions -> FilePath -> IO (Problem Scientific)
readFile opt fname =
  case getExt fname of
    ".lp"  -> readLPFile opt fname
    ".mps" -> readMPSFile opt fname
    ext -> ioError $ userError $ "unknown extension: " ++ ext

-- | Parse a file containing LP file data.
readLPFile :: FileOptions -> FilePath -> IO (Problem Scientific)
#ifndef WITH_ZLIB
readLPFile = LPFile.parseFile
#else
readLPFile opt fname = do
  s <- readTextFile opt fname
  let ret = LPFile.parseString opt fname s
  case ret of
    Left e -> throw e
    Right a -> return a
#endif

-- | Parse a file containing MPS file data.
readMPSFile :: FileOptions -> FilePath -> IO (Problem Scientific)
#ifndef WITH_ZLIB
readMPSFile = MPSFile.parseFile
#else
readMPSFile opt fname = do
  s <- readTextFile opt fname
  let ret = MPSFile.parseString opt fname s
  case ret of
    Left e -> throw e
    Right a -> return a
#endif

readTextFile :: FileOptions -> FilePath -> IO TL.Text
#ifndef WITH_ZLIB
readTextFile opt fname = do
  h <- openFile fname ReadMode
  case optFileEncoding opt of
    Nothing -> return ()
    Just enc -> hSetEncoding h enc
  TLIO.hGetContents h
#else
readTextFile opt fname = do
  enc <- case optFileEncoding opt of
         Nothing -> getLocaleEncoding
         Just enc -> return enc
  let f = if CI.mk (takeExtension fname) == ".gz" then GZip.decompress else id
  s <- BL.readFile fname
  return $ decode enc $ f s
#endif

-- | Parse a string containing LP file data.
parseLPString :: (Stream s, Token s ~ Char, IsString (Tokens s)) => FileOptions -> String -> s -> Either (ParseError s) (Problem Scientific)
parseLPString = LPFile.parseString

-- | Parse a string containing MPS file data.
parseMPSString :: (Stream s, Token s ~ Char, IsString (Tokens s)) => FileOptions -> String -> s -> Either (ParseError s) (Problem Scientific)
parseMPSString = MPSFile.parseString

-- | Generate LP file or MPS file based on file extension.
writeFile :: FileOptions -> FilePath -> Problem Scientific -> IO ()
writeFile opt fname prob =
  case getExt fname of
    ".lp"  -> writeLPFile opt fname prob
    ".mps" -> writeMPSFile opt fname prob
    ext -> ioError $ userError $ "unknown extension: " ++ ext

getExt :: String -> String
getExt fname | (base, ext) <- splitExtension fname =
  case map toLower ext of
#ifdef WITH_ZLIB
    ".gz" -> getExt base
#endif
    s -> s

-- | Generate LP file.
writeLPFile :: FileOptions -> FilePath -> Problem Scientific -> IO ()
writeLPFile opt fname prob =
  case LPFile.render opt{ optNewline = optNewline opt <|> Just nativeNewline } prob of
    Left err -> ioError $ userError err
    Right s -> writeTextFile opt fname s

-- | Generate MPS file.
writeMPSFile :: FileOptions -> FilePath -> Problem Scientific -> IO ()
writeMPSFile opt fname prob =
  case MPSFile.render opt{ optNewline = optNewline opt <|> Just nativeNewline } prob of
    Left err -> ioError $ userError err
    Right s -> writeTextFile opt fname s

writeTextFile :: FileOptions -> FilePath -> TL.Text -> IO ()
writeTextFile opt fname s = do
  let writeSimple = do
        withFile fname WriteMode $ \h -> do
          hSetNewlineMode h noNewlineTranslation
          case optFileEncoding opt of
            Nothing -> return ()
            Just enc -> hSetEncoding h enc
          TLIO.hPutStr h s
#ifdef WITH_ZLIB
  if CI.mk (takeExtension fname) /= ".gz" then do
    writeSimple
  else do
    enc <- case optFileEncoding opt of
             Nothing -> getLocaleEncoding
             Just enc -> return enc
    BL.writeFile fname $ GZip.compress $ encode enc s
#else
  writeSimple
#endif

-- | Generate a 'TL.Text' containing LP file data.
toLPString :: FileOptions -> Problem Scientific -> Either String TL.Text
toLPString opt = LPFile.render opt{ optNewline = optNewline opt <|> Just LF }

-- | Generate a 'TL.Text' containing MPS file data.
toMPSString :: FileOptions -> Problem Scientific -> Either String TL.Text
toMPSString opt = MPSFile.render opt{ optNewline = optNewline opt <|> Just LF }

-- $IO
-- If this library is built with @WithZlib@ flag (enabled by default), 
-- reading/writing gzipped file (@.gz@) are also supported.
