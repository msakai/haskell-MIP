{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.Solution.HiGHS
-- Copyright   :  (c) Masahiro Sakai 2025
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP.Solution.HiGHS
  ( Solution (..)
  , parse
  , readFile
  ) where

import Prelude hiding (readFile, writeFile)

import Control.Exception
import Control.Monad
import Data.Char (isSpace)
import qualified Data.Map as Map
import Data.Scientific (Scientific)
import Data.String
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import System.IO hiding (readFile, writeFile)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec hiding (label, skipManyTill, parse, ParseError)
import Text.Megaparsec.Char hiding (string', char')
import qualified Text.Megaparsec.Char.Lexer as P

import qualified Numeric.Optimization.MIP.Base as MIP
import Numeric.Optimization.MIP.Base (Solution (..))
import Numeric.Optimization.MIP.FileUtils (ParseError)

parser :: forall e s m. (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m (MIP.Solution Scientific)
parser = do
  _ <- strippedLine (string "Model status")
  status <- stripped $ msum $ map Megaparsec.try
    [ string "Not Set" *> pure MIP.StatusUnknown
    , string "Load error" *> pure MIP.StatusUnknown
    , string "Model error" *> pure MIP.StatusUnknown
    , string "Presolve error" *> pure MIP.StatusUnknown
    , string "Solve error" *> pure MIP.StatusUnknown
    , string "Postsolve error" *> pure MIP.StatusUnknown
    , string "Empty" *> pure MIP.StatusUnknown
    , string "Memory limit reached" *> pure MIP.StatusUnknown
    , string "Optimal" *> pure MIP.StatusOptimal
    , string "Infeasible" *> pure MIP.StatusInfeasible
    , string "Primal infeasible or unbounded" *> pure MIP.StatusInfeasibleOrUnbounded
    , string "Unbounded" *> pure MIP.StatusUnbounded
    , string "Bound on objective reached" *> pure MIP.StatusFeasible -- ???
    , string "Target for objective reached" *> pure MIP.StatusFeasible -- ???
    , string "Time limit reached" *> pure MIP.StatusUnknown
    , string "Iteration limit reached" *> pure MIP.StatusUnknown
    , string "Solution limit reached" *> pure MIP.StatusFeasible -- ???
    , string "Interrupted by user" *> pure MIP.StatusUnknown
    , string "Unknown" *> pure MIP.StatusUnknown
    ]
  _ <- eol

  space

  _ <- strippedLine (string "# Primal solution values")
  hspace
  msum
    [ do
        _ <- string "None" <* hspace <* eol
        return $
          MIP.Solution
          { MIP.solStatus = status
          , MIP.solObjectiveValue = Nothing
          , MIP.solVariables = Map.empty
          }
    , do
        _ <- (string "Feasible" <|> string "Infeasible") <* hspace <* eol
        obj <- strippedLine (string "Objective" *> hspace1 *> P.scientific)
        n <- strippedLine (string "# Columns " *> P.decimal)
        values <- replicateM n $ strippedLine ((,) <$> (ident <* hspace1) <*> P.scientific)
        return $
          MIP.Solution
          { MIP.solStatus = status
          , MIP.solObjectiveValue = Just obj
          , MIP.solVariables = Map.fromList values
          }
    ]

  where
    stripped :: m a -> m a
    stripped p = hspace *> p <* hspace

    strippedLine :: m a -> m a
    strippedLine p = stripped p <* eol

    ident :: m MIP.Var
    ident = MIP.toVar <$> some (satisfy (not . isSpace)) <?> "identifier"

parse :: TL.Text -> Either (ParseError TL.Text) (MIP.Solution Scientific)
parse = Megaparsec.parse parser "<string>"

readFile :: FilePath -> IO (MIP.Solution Scientific)
readFile fname = do
  h <- openFile fname ReadMode
  hSetEncoding h utf8
  ret <- Megaparsec.parse parser fname <$> TLIO.hGetContents h
  case ret of
    Left e -> throwIO (e :: ParseError TL.Text)
    Right a -> return a

#if !MIN_VERSION_megaparsec(9,0,0)

hspace :: (MonadParsec e s m, Token s ~ Char) => m ()
hspace = void $ takeWhileP (Just "white space") isHSpace
{-# INLINE hspace #-}

hspace1 :: (MonadParsec e s m, Token s ~ Char) => m ()
hspace1 = void $ takeWhile1P (Just "white space") isHSpace
{-# INLINE hspace1 #-}

isHSpace :: Char -> Bool
isHSpace x = isSpace x && x /= '\n' && x /= '\r'

#endif
