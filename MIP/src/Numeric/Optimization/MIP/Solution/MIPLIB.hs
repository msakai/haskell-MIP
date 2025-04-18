{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.Solution.MIPLIB
-- Copyright   :  (c) Masahiro Sakai 2025
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP.Solution.MIPLIB
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
import Numeric.Optimization.MIP (Solution)
import qualified Numeric.Optimization.MIP as MIP

render :: MIP.Solution Scientific -> TL.Text
render sol = B.toLazyText $ ls1 <> ls2 <> mconcat ls3
  where
    ls1 = if MIP.solStatus sol == MIP.StatusInfeasible then
            "=infeas=" <> B.singleton '\n'
          else
            mempty
    ls2 = case MIP.solObjectiveValue sol of
            Nothing  -> mempty
            Just val -> "=obj= " <> B.scientificBuilder val <> B.singleton '\n'
    ls3 = [ B.fromText (MIP.varName name) <> B.singleton ' ' <> B.scientificBuilder val <> B.singleton '\n'
          | (name,val) <- Map.toList (MIP.solVariables sol)
          ]

writeFile :: FilePath -> MIP.Solution Scientific -> IO ()
writeFile fname sol = do
  TLIO.writeFile fname (render sol)

parse :: TL.Text -> MIP.Solution Scientific
parse t =
  case foldl' f (False,Nothing,[]) $ TL.lines t of
    (infeas, obj, vs) ->
      def{ MIP.solStatus = if infeas then MIP.StatusInfeasible else MIP.StatusFeasible
         , MIP.solObjectiveValue = obj
         , MIP.solVariables = Map.fromList vs
         }
  where
    f :: (Bool, Maybe Scientific, [(MIP.Var, Scientific)]) -> TL.Text -> (Bool, Maybe Scientific, [(MIP.Var, Scientific)])
    f (infeas, obj, vs) l =
      case TL.words l of
        [] -> (infeas, obj, vs)
        "=infeas=" : _ -> (True, obj, vs) 
        [w1, w2] | (r:_) <- [r | (r,[]) <- reads (TL.unpack w2)] ->
          if w1 == "=obj=" then
            (infeas, Just r, vs)
          else
            (infeas, obj, (MIP.Var (TL.toStrict w1), r) : vs)
        _ -> error ("Numeric.Optimization.MIP.Solution.MIPLIB: invalid line " ++ show l)

readFile :: FilePath -> IO (MIP.Solution Scientific)
readFile fname = parse <$> TLIO.readFile fname
