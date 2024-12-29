{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
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

import Data.Void
import qualified Text.Megaparsec as MP

-- | Error type for parsing.
--
-- The definition is slightly different based on the @megaparsec@ version.
type ParseError s = MP.ParseErrorBundle s Void
