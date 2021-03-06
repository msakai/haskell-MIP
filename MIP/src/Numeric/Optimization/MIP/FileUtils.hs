{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.FileUtils
-- Copyright   :  (c) Masahiro Sakai 2018
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP.FileUtils
  ( ParseError
  ) where

#if MIN_VERSION_megaparsec(6,0,0)
import Data.Void
#endif
import qualified Text.Megaparsec as MP

-- | Error type for parsing.
--
-- The definition is slightly different based on the @megaparsec@ version.
#if MIN_VERSION_megaparsec(7,0,0)
type ParseError s = MP.ParseErrorBundle s Void
#elif MIN_VERSION_megaparsec(6,0,0)
type ParseError s = MP.ParseError (MP.Token s) Void
#else
type ParseError s = MP.ParseError (MP.Token s) MP.Dec
#endif
