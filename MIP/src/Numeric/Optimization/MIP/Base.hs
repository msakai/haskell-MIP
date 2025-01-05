{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.Base
-- Copyright   :  (c) Masahiro Sakai 2011-2019
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Mixed-Integer Programming Problems with some commmonly used extensions
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP.Base
  (
  -- * The MIP Problem type
    Problem (..)
  , Label

  -- * Variables
  , Var (Var)
  , varName
  , toVar
  , fromVar

  -- ** Variable types
  , VarType (..)
  , getVarType

  -- ** Variable bounds
  , BoundExpr
  , Extended (..)
  , Bounds
  , defaultBounds
  , defaultLB
  , defaultUB
  , getBounds

  -- ** Variable getters
  , variables
  , integerVariables
  , semiContinuousVariables
  , semiIntegerVariables

  -- * Expressions
  , Expr (..)
  , varExpr
  , constExpr
  , terms
  , Term (..)

  -- * Objective function
  , OptDir (..)
  , ObjectiveFunction (..)

  -- * Constraints

  -- ** Linear (or Quadratic or Polynomial) constraints
  , Constraint (..)
  , (.==.)
  , (.<=.)
  , (.>=.)
  , RelOp (..)

  -- ** SOS constraints
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

  -- * File I/O options
  , FileOptions (..)
  , WriteSetting (..)

  -- * Utilities
  , Default (..)
  , Variables (..)
  , intersectBounds
  ) where

#if !MIN_VERSION_lattices(2,0,0)
import Algebra.Lattice
#endif
import Algebra.PartialOrd
import Control.Arrow ((***))
import Control.Monad
import Data.Default.Class
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Interned (intern, unintern)
import Data.Interned.Text
import Data.ExtendedReal
import Data.OptDir
import Data.String
import qualified Data.Text as T
import System.IO (TextEncoding)

infix 4 .<=., .>=., .==.

-- ---------------------------------------------------------------------------

-- | Problem
data Problem c
  = Problem
  { name :: Maybe T.Text
  , objectiveFunction :: ObjectiveFunction c
  , constraints :: [Constraint c]
  , sosConstraints :: [SOSConstraint c]
  , userCuts :: [Constraint c]
  , varType :: Map Var VarType
  , varBounds :: Map Var (Bounds c)
  }
  deriving (Show, Eq, Ord)

instance Default (Problem c) where
  def = Problem
        { name = Nothing
        , objectiveFunction = def
        , constraints = []
        , sosConstraints = []
        , userCuts = []
        , varType = Map.empty
        , varBounds = Map.empty
        }

instance Functor Problem where
  fmap f prob =
    prob
    { objectiveFunction = fmap f (objectiveFunction prob)
    , constraints       = map (fmap f) (constraints prob)
    , sosConstraints    = map (fmap f) (sosConstraints prob)
    , userCuts          = map (fmap f) (userCuts prob)
    , varBounds         = fmap (fmap f *** fmap f) (varBounds prob)
    }

-- | label
type Label = T.Text

-- ---------------------------------------------------------------------------

-- | variable
newtype Var = Var' InternedText
  deriving Eq

pattern Var :: T.Text -> Var
pattern Var s <- Var' (unintern -> s) where
  Var s = Var' (intern s)

{-# COMPLETE Var #-}

instance IsString Var where
  fromString = Var' . fromString

instance Ord Var where
  compare (Var' a) (Var' b)
    | a == b = EQ
    | otherwise = compare (unintern a) (unintern b)

instance Show Var where
  showsPrec d (Var x) = showsPrec d x

-- | Variable's name
varName :: Var -> T.Text
varName (Var s) = s

{-# DEPRECATED toVar "Use fromString function or Var pattern instead" #-}
-- | convert a string into a variable
toVar :: String -> Var
toVar = fromString

{-# DEPRECATED fromVar "Use varName function or Var pattern instead" #-}
-- | convert a variable into a string
fromVar :: Var -> String
fromVar (Var s) = T.unpack s

data VarType
  = ContinuousVariable
  | IntegerVariable
  | SemiContinuousVariable
  | SemiIntegerVariable
  deriving (Eq, Ord, Show)

instance Default VarType where
  def = ContinuousVariable

-- | looking up bounds for a variable
getVarType :: Problem c -> Var -> VarType
getVarType mip v = Map.findWithDefault def v (varType mip)

-- | type for representing lower/upper bound of variables
type BoundExpr c = Extended c

-- | type for representing lower/upper bound of variables
type Bounds c = (BoundExpr c, BoundExpr c)

-- | default bounds
defaultBounds :: Num c => Bounds c
defaultBounds = (defaultLB, defaultUB)

-- | default lower bound (0)
defaultLB :: Num c => BoundExpr c
defaultLB = Finite 0

-- | default upper bound (+∞)
defaultUB :: BoundExpr c
defaultUB = PosInf

-- | looking up bounds for a variable
getBounds :: Num c => Problem c -> Var -> Bounds c
getBounds mip v = Map.findWithDefault defaultBounds v (varBounds mip)

intersectBounds :: Ord c => Bounds c -> Bounds c -> Bounds c
intersectBounds (lb1,ub1) (lb2,ub2) = (max lb1 lb2, min ub1 ub2)

-- ---------------------------------------------------------------------------

-- | expressions
newtype Expr c = Expr [Term c]
  deriving (Eq, Ord, Show)

varExpr :: Num c => Var -> Expr c
varExpr v = Expr [Term 1 [v]]

constExpr :: (Eq c, Num c) => c -> Expr c
constExpr 0 = Expr []
constExpr c = Expr [Term c []]

terms :: Expr c -> [Term c]
terms (Expr ts) = ts

instance Num c => Num (Expr c) where
  Expr e1 + Expr e2 = Expr (e1 ++ e2)
  Expr e1 * Expr e2 = Expr [Term (c1*c2) (vs1 ++ vs2) | Term c1 vs1 <- e1, Term c2 vs2 <- e2]
  negate (Expr e) = Expr [Term (-c) vs | Term c vs <- e]
  abs = id
  signum _ = 1
  fromInteger 0 = Expr []
  fromInteger c = Expr [Term (fromInteger c) []]

instance Functor Expr where
  fmap f (Expr ts) = Expr $ map (fmap f) ts

splitConst :: Num c => Expr c -> (Expr c, c)
splitConst e = (e2, c2)
  where
    e2 = Expr [t | t@(Term _ (_:_)) <- terms e]
    c2 = sum [c | Term c [] <- terms e]

-- | terms
data Term c = Term c [Var]
  deriving (Eq, Ord, Show)

instance Functor Term where
  fmap f (Term c vs) = Term (f c) vs

-- ---------------------------------------------------------------------------

-- | objective function
data ObjectiveFunction c
  = ObjectiveFunction
  { objLabel :: Maybe Label
  , objDir :: OptDir
  , objExpr :: Expr c
  }
  deriving (Eq, Ord, Show)

instance Default (ObjectiveFunction c) where
  def =
    ObjectiveFunction
    { objLabel = Nothing
    , objDir = OptMin
    , objExpr = Expr []
    }

instance Functor ObjectiveFunction where
  fmap f obj = obj{ objExpr = fmap f (objExpr obj) }

-- ---------------------------------------------------------------------------

-- | constraint
data Constraint c
  = Constraint
  { constrLabel     :: Maybe Label
  , constrIndicator :: Maybe (Var, c)
  , constrExpr      :: Expr c
  , constrLB        :: BoundExpr c
  , constrUB        :: BoundExpr c
  , constrIsLazy    :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Equality constraint.
(.==.) :: Num c => Expr c -> Expr c -> Constraint c
lhs .==. rhs =
  case splitConst (lhs - rhs) of
    (e, c) -> def{ constrExpr = e, constrLB = Finite (- c), constrUB = Finite (- c) }

-- | Inequality constraint (≤).
(.<=.) :: Num c => Expr c -> Expr c -> Constraint c
lhs .<=. rhs =
  case splitConst (lhs - rhs) of
    (e, c) -> def{ constrExpr = e, constrUB = Finite (- c) }

-- | Inequality constraint (≥).
(.>=.) :: Num c => Expr c -> Expr c -> Constraint c
lhs .>=. rhs =
  case splitConst (lhs - rhs) of
    (e, c) -> def{ constrExpr = e, constrLB = Finite (- c) }

instance Default (Constraint c) where
  def = Constraint
        { constrLabel = Nothing
        , constrIndicator = Nothing
        , constrExpr = Expr []
        , constrLB = NegInf
        , constrUB = PosInf
        , constrIsLazy = False
        }

instance Functor Constraint where
  fmap f c =
    c
    { constrIndicator = fmap (id *** f) (constrIndicator c)
    , constrExpr = fmap f (constrExpr c)
    , constrLB = fmap f (constrLB c)
    , constrUB = fmap f (constrUB c)
    }

-- | relational operators
data RelOp = Le | Ge | Eql
  deriving (Eq, Ord, Enum, Show)

-- ---------------------------------------------------------------------------

-- | types of SOS (special ordered sets) constraints
data SOSType
  = S1 -- ^ Type 1 SOS constraint
  | S2 -- ^ Type 2 SOS constraint
  deriving (Eq, Ord, Enum, Show, Read)

-- | SOS (special ordered sets) constraints
data SOSConstraint c
  = SOSConstraint
  { sosLabel :: Maybe Label
  , sosType  :: SOSType
  , sosBody  :: [(Var, c)]
  }
  deriving (Eq, Ord, Show)

instance Functor SOSConstraint where
  fmap f c = c{ sosBody = map (id *** f) (sosBody c) }

-- ---------------------------------------------------------------------------

-- | MIP status with the following partial order:
--
-- <<doc-images/MIP-Status-diagram.png>>
data Status
  = StatusUnknown
  | StatusFeasible
  | StatusOptimal
  | StatusInfeasibleOrUnbounded
  | StatusInfeasible
  | StatusUnbounded
  deriving (Eq, Ord, Enum, Bounded, Show)

instance PartialOrd Status where
  leq a b = (a,b) `Set.member` rel
    where
      rel = unsafeLfpFrom rel0 $ \r ->
        Set.union r (Set.fromList [(x,z) | (x,y) <- Set.toList r, (y',z) <- Set.toList r, y == y'])
      rel0 = Set.fromList $
        [(x,x) | x <- [minBound .. maxBound]] ++
        [ (StatusUnknown, StatusFeasible)
        , (StatusUnknown, StatusInfeasibleOrUnbounded)
        , (StatusFeasible, StatusOptimal)
        , (StatusFeasible, StatusUnbounded)
        , (StatusInfeasibleOrUnbounded, StatusUnbounded)
        , (StatusInfeasibleOrUnbounded, StatusInfeasible)
        ]

-- | /meet/ (greatest lower bound) operator of the partial order of 'Status' type.
--
-- If the version of @lattices@ is \<2, then @MeetSemiLattice@ instance can also be used.
meetStatus :: Status -> Status -> Status
StatusUnknown `meetStatus` _b = StatusUnknown
StatusFeasible `meetStatus` b
  | StatusFeasible `leq` b = StatusFeasible
  | otherwise = StatusUnknown
StatusOptimal `meetStatus` StatusOptimal = StatusOptimal
StatusOptimal `meetStatus` b
  | StatusFeasible `leq` b = StatusFeasible
  | otherwise = StatusUnknown
StatusInfeasibleOrUnbounded `meetStatus` b
  | StatusInfeasibleOrUnbounded `leq` b = StatusInfeasibleOrUnbounded
  | otherwise = StatusUnknown
StatusInfeasible `meetStatus` StatusInfeasible = StatusInfeasible
StatusInfeasible `meetStatus` b
  | StatusInfeasibleOrUnbounded `leq` b = StatusInfeasibleOrUnbounded
  | otherwise = StatusUnknown
StatusUnbounded `meetStatus` StatusUnbounded = StatusUnbounded
StatusUnbounded `meetStatus` b
  | StatusFeasible `leq` b = StatusFeasible
  | StatusInfeasibleOrUnbounded `leq` b = StatusInfeasibleOrUnbounded
  | otherwise = StatusUnknown

#if !MIN_VERSION_lattices(2,0,0)

instance MeetSemiLattice Status where
  meet = meetStatus

#endif


-- | Type for representing a solution of MIP problem.
data Solution r
  = Solution
  { solStatus :: Status
  , solObjectiveValue :: Maybe r
  , solVariables :: Map Var r
  }
  deriving (Eq, Ord, Show)

instance Functor Solution where
  fmap f (Solution status obj vs) = Solution status (fmap f obj) (fmap f vs)

instance Default (Solution r) where
  def = Solution
        { solStatus = StatusUnknown
        , solObjectiveValue = Nothing
        , solVariables = Map.empty
        }

-- ---------------------------------------------------------------------------

data Tol r
  = Tol
  { integralityTol :: r
  , feasibilityTol :: r
  , optimalityTol :: r
  }

-- | Defautl is @1e-6@ for the feasibility and optimality tolerances, and @1e-5@ for the integrality tolerance.
instance Fractional r => Default (Tol r) where
  def =
    Tol
    { integralityTol = 1e-5
    , feasibilityTol = 1e-6
    , optimalityTol = 1e-6
    }

zeroTol :: Fractional r => Tol r
zeroTol =
  Tol
  { integralityTol = 1e-5
  , feasibilityTol = 1e-6
  , optimalityTol = 1e-6
  }

class Eval r a where
  type Evaluated r a
  eval :: Tol r -> Map Var r -> a -> Evaluated r a

instance Num r => Eval r Var where
  type Evaluated r Var = r
  eval _tol sol v =
    case Map.lookup v sol of
      Just val -> val
      Nothing -> 0

instance Num r => Eval r (Term r) where
  type Evaluated r (Term r) = r
  eval tol sol (Term c vs) = product (c : [eval tol sol v | v <- vs])

instance Num r => Eval r (Expr r) where
  type Evaluated r (Expr r) = r
  eval tol sol expr = sum [eval tol sol t | t <- terms expr]

instance Num r => Eval r (ObjectiveFunction r) where
  type Evaluated r (ObjectiveFunction r) = r
  eval tol sol obj = eval tol sol (objExpr obj)

instance (Num r, Ord r) => Eval r (Constraint r) where
  type Evaluated r (Constraint r) = Bool
  eval tol sol constr =
    not (evalIndicator (constrIndicator constr)) ||
    isInBounds tol (constrLB constr, constrUB constr) (eval tol sol (constrExpr constr))
    where
      evalIndicator Nothing = True
      evalIndicator (Just (v, val')) = isInBounds tol (Finite val', Finite val') (eval tol sol v)

instance (Num r, Ord r) => Eval r (SOSConstraint r) where
  type Evaluated r (SOSConstraint r) = Bool
  eval tol sol sos =
    case sosType sos of
      S1 -> length [() | val <- body, val] <= 1
      S2 -> f body
    where
      body = map (not . isInBounds tol (0, 0) . eval tol sol . fst) $ sortBy (comparing snd) $ (sosBody sos)
      f [] = True
      f [_] = True
      f (x1 : x2 : xs)
        | x1 = all not xs
        | otherwise = f (x2 : xs)

instance (RealFrac r) => Eval r (Problem r) where
  type Evaluated r (Problem r) = Maybe r
  eval tol sol prob = do
    forM_ (Map.toList (Map.intersectionWith (,) (varType prob) (varBounds prob))) $ \(v, (vt, bounds)) -> do
      let val = eval tol sol v
      case vt of
        ContinuousVariable -> return ()
        SemiContinuousVariable -> return ()
        IntegerVariable -> guard $ isIntegral tol val
        SemiIntegerVariable -> guard $ isIntegral tol val
      case vt of
        ContinuousVariable -> guard $ isInBounds tol bounds val
        IntegerVariable -> guard $ isInBounds tol bounds val
        SemiIntegerVariable -> guard $ isInBounds tol (0,0) val || isInBounds tol bounds val
        SemiContinuousVariable -> guard $ isInBounds tol (0,0) val || isInBounds tol bounds val
    forM_ (constraints prob) $ \constr -> do
      guard $ eval tol sol constr
    forM_ (sosConstraints prob) $ \constr -> do
      guard $ eval tol sol constr
    return $ eval tol sol (objectiveFunction prob)

isIntegral :: RealFrac r => Tol r -> r -> Bool
isIntegral tol x = abs (x - fromIntegral (floor (x + 0.5) :: Integer)) <= integralityTol tol

isInBounds :: (Num r, Ord r) => Tol r -> Bounds r -> r -> Bool
isInBounds tol (lb, ub) x =
  lb - Finite (feasibilityTol tol) <= Finite x &&
  Finite x <= ub + Finite (feasibilityTol tol)

-- ---------------------------------------------------------------------------

class Variables a where
  vars :: a -> Set Var

instance Variables a => Variables [a] where
  vars = Set.unions . map vars

instance (Variables a, Variables b) => Variables (Either a b) where
  vars (Left a)  = vars a
  vars (Right b) = vars b

instance Variables (Problem c) where
  vars = variables

instance Variables (Expr c) where
  vars (Expr e) = vars e

instance Variables (Term c) where
  vars (Term _ xs) = Set.fromList xs

instance Variables (ObjectiveFunction c) where
  vars ObjectiveFunction{ objExpr = e } = vars e

instance Variables (Constraint c) where
  vars Constraint{ constrIndicator = ind, constrExpr = e } = Set.union (vars e) vs2
    where
      vs2 = maybe Set.empty (Set.singleton . fst) ind

instance Variables (SOSConstraint c) where
  vars SOSConstraint{ sosBody = xs } = Set.fromList (map fst xs)

-- ---------------------------------------------------------------------------

variables :: Problem c -> Set Var
variables mip = Map.keysSet $ varType mip

integerVariables :: Problem c -> Set Var
integerVariables mip = Map.keysSet $ Map.filter (IntegerVariable ==) (varType mip)

semiContinuousVariables :: Problem c -> Set Var
semiContinuousVariables mip = Map.keysSet $ Map.filter (SemiContinuousVariable ==) (varType mip)

semiIntegerVariables :: Problem c -> Set Var
semiIntegerVariables mip = Map.keysSet $ Map.filter (SemiIntegerVariable ==) (varType mip)

-- ---------------------------------------------------------------------------

data FileOptions
  = FileOptions
  { optFileEncoding :: Maybe TextEncoding
  , optMPSWriteObjSense :: WriteSetting
  } deriving (Show)

instance Default FileOptions where
  def =
    FileOptions
    { optFileEncoding = Nothing
    , optMPSWriteObjSense = WriteIfNotDefault
    }

data WriteSetting
  = WriteAlways
  | WriteIfNotDefault
  | WriteNever
  deriving (Eq, Ord, Enum, Bounded, Show, Read)
