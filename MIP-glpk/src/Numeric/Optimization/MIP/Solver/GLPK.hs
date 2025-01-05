{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.Solver.GLPK
-- Copyright   :  (c) Masahiro Sakai 2020
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP.Solver.GLPK
  ( GLPK (..)
  , glpk
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString.Encoding (encode, localeEncoding)
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import qualified Data.Set as Set
import qualified Data.Text as T
import Foreign
import Foreign.C

import Data.ExtendedReal

import qualified Numeric.Optimization.MIP as MIP
import Numeric.Optimization.MIP.Solver.Base
import qualified Math.Programming.Glpk.Header as Raw

data GLPK
  = GLPK

instance Default GLPK where
  def = GLPK

glpk :: GLPK
glpk = GLPK

instance IsSolver GLPK IO where
  solve = solve'
  
  solve' _solver opt prob =
    (if rtsSupportsBoundThreads then runInBoundThread else id) $
    bracket Raw.glp_init_env (\ret -> when (ret == 0) $ Raw.glp_free_env >> return ()) $ \_ -> do
    bracket Raw.glp_create_prob Raw.glp_delete_prob $ \prob' -> do

      let vs = MIP.variables  prob
          varToCol = Map.fromList $ zip (Set.toAscList vs) [1..]
          exprToMap (MIP.Expr ts) = Map.fromListWith (+) $ do
            t <- ts
            case t of
              MIP.Term c [] -> return (0, c)
              MIP.Term c [v] -> return (varToCol Map.! v, c)
              MIP.Term _ _ -> error "GLPK does not support non-linear term"

      case MIP.name prob of
        Nothing -> return ()
        Just name -> useTextAsCString name (Raw.glp_set_prob_name prob')

      -- Variables
      _ <- Raw.glp_add_cols prob' $ fromIntegral $ Map.size $ MIP.varType prob
      forM_ (Map.toList varToCol) $ \(v, col) -> do
        let (lb, ub) = MIP.getBounds prob v
        useTextAsCString (MIP.varName v) (Raw.glp_set_col_name prob' col)
        Raw.glp_set_col_kind prob' col $
          case MIP.getVarType prob v of
            MIP.SemiContinuousVariable -> error "GLPK does not support semi-continuous variables"
            MIP.SemiIntegerVariable -> error "GLPK does not support semi-integer variables"
            MIP.ContinuousVariable -> Raw.glpkContinuous
            MIP.IntegerVariable ->
              case (lb, ub) of
                (Finite 0, Finite 1) -> Raw.glpkBinary
                _ -> Raw.glpkInteger
        case fromBound lb ub of
          (constrType, lb', ub') -> Raw.glp_set_col_bnds prob' col constrType lb' ub'

      -- Objective Function
      let obj = MIP.objectiveFunction prob
      Raw.glp_set_obj_dir prob' $
        case MIP.objDir obj of
          MIP.OptMax -> Raw.glpkMax
          MIP.OptMin -> Raw.glpkMin
      case MIP.objLabel obj of
        Nothing -> return ()
        Just name -> useTextAsCString name (Raw.glp_set_obj_name prob')
      forM_ (Map.toList (exprToMap (MIP.objExpr obj))) $ \(col, c) -> do
        Raw.glp_set_obj_coef prob' col (toRealFloat c)

      -- Constraints
      let constrs = MIP.constraints prob
      _ <- Raw.glp_add_rows prob' $ fromIntegral $ length constrs
      forM_ (zip [1..] constrs) $ \(row, constr) -> do
        case MIP.constrIndicator constr of
          Nothing -> return ()
          Just _ -> error "Indicator constraints are not supported"
        when (MIP.constrIsLazy constr) $ do
          error "GLPK does not support lazy constraints"
        case MIP.constrLabel constr of
          Nothing -> return ()
          Just name -> useTextAsCString name (Raw.glp_set_row_name prob' row)
        case fromBound (MIP.constrLB constr) (MIP.constrUB constr) of
          (constrType, lb', ub') -> Raw.glp_set_row_bnds prob' row constrType lb' ub'
        -- TODO: check constant terms
        let m = exprToMap (MIP.constrExpr constr)
            ts = Map.toList m
            n = Map.size m
        Raw.allocaGlpkArray (map fst ts) $ \ind -> do
          Raw.allocaGlpkArray (map (toRealFloat . snd) ts) $ \val -> do
            Raw.glp_set_mat_row prob' row (fromIntegral n) ind val

      when (length (MIP.sosConstraints prob) > 0) $ do
        error "GLPK does not support SOS constraints"

      when (length (MIP.userCuts prob) > 0) $ do
        error "GLPK does not support user cuts"

      let loggingCallback :: Ptr () -> CString -> IO CInt
          loggingCallback _ p = do
            s <- peekCString p
            solveLogger opt s
            return 1

      -- Solving
      alloca $ \p -> do
        Raw.glp_init_iocp p
        iocp <- peek p
        poke p $
          iocp
          { Raw.iocpPresolve = Raw.glpkPresolveOn
          , Raw.iocpTimeLimitMillis =
              case solveTimeLimit opt of
                Nothing -> Raw.iocpTimeLimitMillis iocp -- maxBound :: CInt
                Just sec -> round (sec * 1000)
          }

        status <-
          bracket (wrapTermHook loggingCallback) freeHaskellFunPtr $ \loggingCallbackPtr -> do
            bracket_ (Raw.glp_term_hook loggingCallbackPtr nullPtr) (Raw.glp_term_hook nullFunPtr nullPtr) $
              Raw.glp_intopt prob' p

        objVal <- liftM fromFloatDigits $ Raw.glp_mip_obj_val prob'
        varVal <- mapM (liftM fromFloatDigits . Raw.glp_mip_col_val prob') varToCol
        let sol = MIP.Solution
              { MIP.solStatus =
                  if status == Raw.glpkMIPSuccess then MIP.StatusOptimal -- ???
                  else if status == Raw.glpkMIPBadBound then MIP.StatusInfeasible
                  else if status == Raw.glpkMIPNoBasis then MIP.StatusUnknown
                  else if status == Raw.glpkMIPPrimalInfeasible then MIP.StatusInfeasible
                  else if status == Raw.glpkMIPDualInfeasible then MIP.StatusInfeasibleOrUnbounded
                  else if status == Raw.glpkMIPFailure then MIP.StatusUnknown
                  else if status == Raw.glpkMIPRelativeGap then MIP.StatusUnknown -- ???
                  else if status == Raw.glpkMIPTimeLimit then MIP.StatusUnknown -- ???
                  else if status == Raw.glpkMIPStopped then MIP.StatusUnknown -- ???
                  else error ("unknown mip status: " ++ show status)
              , MIP.solObjectiveValue = Just objVal
              , MIP.solVariables = varVal
              }
        return sol

fromBound :: MIP.BoundExpr Scientific -> MIP.BoundExpr Scientific -> (Raw.GlpkConstraintType, CDouble, CDouble)
fromBound NegInf PosInf = (Raw.glpkFree, 0, 0)
fromBound (Finite lb') (Finite ub')
  | lb' == ub' = (Raw.glpkFixed, toRealFloat lb', toRealFloat ub')
  | otherwise  = (Raw.glpkBounded, toRealFloat lb', toRealFloat ub')
fromBound (Finite lb') PosInf = (Raw.glpkGT, toRealFloat lb', 0)
fromBound NegInf (Finite ub') = (Raw.glpkLT, 0, toRealFloat ub')
fromBound _ NegInf = (Raw.glpkBounded, 1, 0)  -- inconsistent
fromBound PosInf _ = (Raw.glpkBounded, 1, 0)  -- inconsistent

useTextAsCString :: T.Text -> (CString -> IO a) -> IO a
useTextAsCString s = B.useAsCString (encode localeEncoding s)

foreign import ccall "wrapper"
  wrapTermHook :: (Ptr a -> CString -> IO CInt) -> IO (FunPtr (Ptr a -> CString -> IO CInt))
