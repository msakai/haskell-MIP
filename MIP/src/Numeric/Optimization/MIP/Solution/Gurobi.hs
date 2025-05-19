{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.Solution.Gurobi
-- Copyright   :  (c) Masahiro Sakai 2012,2017
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP.Solution.Gurobi
  ( Solution (..)
  , render
  , writeFile
  , parse
  , readFile
  ) where

import Prelude hiding (readFile, writeFile)
import Data.Default.Class
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif
import qualified Data.Map as Map
import Data.Scientific (Scientific)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Scientific as B
import qualified Data.Text.Lazy.IO as TLIO
import Numeric.Optimization.MIP.Base (Solution)
import qualified Numeric.Optimization.MIP.Base as MIP

render :: MIP.Solution Scientific -> TL.Text
render sol = B.toLazyText $ ls1 <> mconcat ls2
  where
    ls1 = case MIP.solObjectiveValue sol of
            Nothing  -> mempty
            Just val -> "# Objective value = " <> B.scientificBuilder val <> B.singleton '\n'
    ls2 = [ B.fromText (MIP.varName name) <> B.singleton ' ' <> B.scientificBuilder val <> B.singleton '\n'
          | (name,val) <- Map.toList (MIP.solVariables sol)
          ]

writeFile :: FilePath -> MIP.Solution Scientific -> IO ()
writeFile fname sol = do
  TLIO.writeFile fname (render sol)

parse :: TL.Text -> MIP.Solution Scientific
parse t =
  case foldl' f (Nothing,[]) $ TL.lines t of
    (obj, vs) ->
      def{ MIP.solStatus = MIP.StatusFeasible
         , MIP.solObjectiveValue = obj
         , MIP.solVariables = Map.fromList vs
         }
  where
    f :: (Maybe Scientific, [(MIP.Var, Scientific)]) -> TL.Text -> (Maybe Scientific, [(MIP.Var, Scientific)])
    f (obj,vs) l
      | Just l2 <- TL.stripPrefix "# " l
      , Just l3 <- TL.stripPrefix "objective value = " (TL.toLower l2)
      , (r:_) <- [r | (r,[]) <- reads (TL.unpack l3)] =
          (Just r, vs)
      | otherwise =
          case TL.words (TL.takeWhile (/= '#') l) of
            [w1, w2] -> (obj, (MIP.Var (TL.toStrict w1), read (TL.unpack w2)) : vs)
            [] -> (obj, vs)
            _ -> error ("Numeric.Optimization.MIP.Solution.Gurobi: invalid line " ++ show l)

readFile :: FilePath -> IO (MIP.Solution Scientific)
readFile fname = parse <$> TLIO.readFile fname
