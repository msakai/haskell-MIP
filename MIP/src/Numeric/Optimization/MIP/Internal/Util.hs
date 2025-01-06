{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.Internal.Util
-- Copyright   :  (c) Masahiro Sakai 2011-2012
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Some utility functions.
--
-----------------------------------------------------------------------------

module Numeric.Optimization.MIP.Internal.Util where

-- | Combining two @Maybe@ values using given function.
combineMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
combineMaybe _ Nothing y = y
combineMaybe _ x Nothing = x
combineMaybe f (Just x) (Just y) = Just (f x y)
