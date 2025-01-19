{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
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
  , Var
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
  , Expr (Expr)
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

  -- * File I/O options
  , FileOptions (..)

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
import Data.Default.Class
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Interned (unintern)
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
type Var = InternedText

-- | convert a string into a variable
toVar :: String -> Var
toVar = fromString

-- | convert a variable into a string
fromVar :: Var -> String
fromVar = T.unpack . unintern

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
newtype Expr c = Expr' (Seq (Term c))
  deriving (Eq, Ord)

pattern Expr :: [Term c] -> Expr c
pattern Expr ts <- Expr' (toList -> ts) where
  Expr ts = Expr' (Seq.fromList ts)

{-# COMPLETE Expr #-}

instance Show c => Show (Expr c) where
  showsPrec d (Expr ts) = showParen (d > app_prec) $
    showString "Expr " . showsPrec (app_prec+1) ts
    where
      app_prec = 10

varExpr :: Num c => Var -> Expr c
varExpr v = Expr' $ Seq.singleton $ Term 1 [v]

constExpr :: (Eq c, Num c) => c -> Expr c
constExpr 0 = Expr' Seq.empty
constExpr c = Expr' $ Seq.singleton $ Term c []

terms :: Expr c -> [Term c]
terms (Expr ts) = ts

instance Num c => Num (Expr c) where
  Expr' e1 + Expr' e2 = Expr' (e1 <> e2)
  Expr e1 * Expr e2 = Expr [Term (c1*c2) (vs1 ++ vs2) | Term c1 vs1 <- e1, Term c2 vs2 <- e2]
  negate (Expr' e) = Expr' $ fmap (\(Term c vs) -> Term (-c) vs) e
  abs = id
  signum _ = 1
  fromInteger 0 = Expr []
  fromInteger c = Expr [Term (fromInteger c) []]

instance Functor Expr where
  fmap f (Expr' ts) = Expr' $ fmap (fmap f) ts

splitConst :: Num c => Expr c -> (Expr c, c)
splitConst (Expr' ts) = (e2, c2)
  where
    p (Term _ (_:_)) = True
    p _ = False
    e2 = Expr' $ Seq.filter p ts
    c2 = sum [c | Term c [] <- toList ts]

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
  } deriving (Show)

instance Default FileOptions where
  def =
    FileOptions
    { optFileEncoding = Nothing
    }
