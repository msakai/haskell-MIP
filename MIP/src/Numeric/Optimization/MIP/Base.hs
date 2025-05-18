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
  , Var (Var', Var)
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
  , SOSType (SOS1, SOS2, S1, S2)
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
  , FileOptions (..)
  , WriteSetting (..)

  -- * Utilities
  , Default (..)
  , Variables (..)
  , intersectBounds
  , isAscii
  ) where

#if !MIN_VERSION_lattices(2,0,0)
import Algebra.Lattice
#endif
import Algebra.PartialOrd
import Control.Arrow ((***), second)
import Control.Monad
#if !MIN_VERSION_text(2,0,2)
import qualified Data.Char as Char
#endif
import Data.Default.Class
import Data.Foldable (toList)
import Data.Hashable
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Ord (comparing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Interned (intern, unintern)
import Data.Interned.Text
import Data.ExtendedReal
import Data.OptDir
import Data.String
import qualified Data.Text as T
import System.IO (Newline (..), TextEncoding)

infix 4 .<=., .>=., .==.

-- ---------------------------------------------------------------------------

-- | A problem instance.
data Problem c
  = Problem
  { name :: Maybe T.Text
    -- ^ Problem name
  , objectiveFunction :: ObjectiveFunction c
    -- ^ Objective functions of the problem
  , constraints :: [Constraint c]
    -- ^ Constraints of the problem
    --
    -- Indicator constraints and lazy constraints are included in this list.
  , sosConstraints :: [SOSConstraint c]
    -- ^ Special ordered sets
  , userCuts :: [Constraint c]
    -- ^ User cuts
  , varDomains :: Map Var (Domain c)
    -- ^ Variable domains
  }
  deriving (Show, Eq, Ord)

instance Default (Problem c) where
  def = Problem
        { name = Nothing
        , objectiveFunction = def
        , constraints = []
        , sosConstraints = []
        , userCuts = []
        , varDomains = Map.empty
        }

instance Functor Problem where
  fmap f prob =
    prob
    { objectiveFunction = fmap f (objectiveFunction prob)
    , constraints       = map (fmap f) (constraints prob)
    , sosConstraints    = map (fmap f) (sosConstraints prob)
    , userCuts          = map (fmap f) (userCuts prob)
    , varDomains        = fmap (second (fmap f *** fmap f)) (varDomains prob)
    }

-- | Types of variables.
--
-- This is equivalent to:
--
-- @
-- 'fmap' 'fst' . 'varDomains'
-- @
varTypes :: Problem c -> Map Var VarType
varTypes = fmap fst . varDomains

{-# DEPRECATED varType "Use varTypes instead" #-}
-- | Types of variables.
--
-- Deprecated alias of 'varTypes'.
varType :: Problem c -> Map Var VarType
varType = varTypes

-- | Bounds of variables.
--
-- This is equivalent to:
--
-- @
-- 'fmap' 'snd' . 'varDomains'
-- @
varBounds :: Problem c -> Map Var (Bounds c)
varBounds = fmap snd . varDomains

-- | Label used for naming various elements of t'Problem'.
type Label = T.Text

-- ---------------------------------------------------------------------------

-- | Variables used in a t'Problem'.
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

instance Hashable Var where
#if MIN_VERSION_intern(0,9,3)
  hashWithSalt salt (Var' x) = hashWithSalt salt x
#else
  hashWithSalt salt (Var' x) = hashWithSalt salt (internedTextId x)
#endif

-- | Variable's name.
varName :: Var -> T.Text
varName (Var s) = s

{-# DEPRECATED toVar "Use fromString function or Var pattern instead" #-}
-- | Convert a string into a variable.
toVar :: String -> Var
toVar = fromString

{-# DEPRECATED fromVar "Use varName function or Var pattern instead" #-}
-- | Convert a variable into a string.
fromVar :: Var -> String
fromVar (Var s) = T.unpack s


-- | Domain of a variable consists of variable type ('VarType') and bounds ('Bounds').
--
-- @since 0.2.1.0
type Domain c = (VarType, Bounds c)

-- | Variable types.
--
-- Variables can take values depending on their types and their bounds ('Bounds').
data VarType
  = ContinuousVariable     -- ^ can take values from \(\{x \in \mathbb{R} \mid L \le x \le U\}\)
  | IntegerVariable        -- ^ can take values from \(\{x \in \mathbb{Z} \mid L \le x \le U\}\)
  | SemiContinuousVariable -- ^ can take values from \(\{0\} \cup \{x \in \mathbb{R} \mid L \le x \le U\}\)
  | SemiIntegerVariable    -- ^ can take values from \(\{0\} \cup \{x \in \mathbb{Z} \mid L \le x \le U\}\)
  deriving (Eq, Ord, Show)

instance Default VarType where
  def = ContinuousVariable

-- | Look up variable type.
getVarType :: Problem c -> Var -> VarType
getVarType mip v =
  case Map.lookup v (varDomains mip) of
    Just (vt, _) -> vt
    Nothing -> def

-- | Type for representing lower/upper bound of variables.
type BoundExpr c = Extended c

-- | Type for representing lower/upper bound of variables.
type Bounds c = (BoundExpr c, BoundExpr c)

-- | Default bounds.
defaultBounds :: Num c => Bounds c
defaultBounds = (defaultLB, defaultUB)

-- | Default lower bound (0).
defaultLB :: Num c => BoundExpr c
defaultLB = Finite 0

-- | Default upper bound (+∞).
defaultUB :: BoundExpr c
defaultUB = PosInf

-- | Look up bounds for a variable.
getBounds :: Num c => Problem c -> Var -> Bounds c
getBounds mip v =
  case Map.lookup v (varDomains mip) of
    Just (_, bs) -> bs
    Nothing -> defaultBounds

-- | Intersection of two 'Bounds'.
intersectBounds :: Ord c => Bounds c -> Bounds c -> Bounds c
intersectBounds (lb1,ub1) (lb2,ub2) = (max lb1 lb2, min ub1 ub2)

-- ---------------------------------------------------------------------------

-- | Arithmetic expressions.
--
-- Essentialy an expression is a sequence of t'Term's.
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

-- | Variable expression.
varExpr :: Num c => Var -> Expr c
varExpr v = Expr' $ Seq.singleton $ Term 1 [v]

-- | Constant expression.
constExpr :: (Eq c, Num c) => c -> Expr c
constExpr 0 = Expr' Seq.empty
constExpr c = Expr' $ Seq.singleton $ Term c []

-- | Terms of an expression.
terms :: Expr c -> [Term c]
terms (Expr ts) = ts

instance Num c => Num (Expr c) where
  Expr' e1 + Expr' e2 = Expr' (e1 <> e2)
  Expr e1 * Expr e2 = Expr [Term (c1*c2) (vs1 ++ vs2) | Term c1 vs1 <- e1, Term c2 vs2 <- e2]
  negate (Expr' e) = Expr' $ fmap (\(Term c vs) -> Term (-c) vs) e
  abs = id
  signum _ = 1
  fromInteger 0 = Expr' Seq.empty
  fromInteger c = Expr' $ Seq.singleton $ Term (fromInteger c) []

instance Functor Expr where
  fmap f (Expr' ts) = Expr' $ fmap (fmap f) ts

-- | Split an expression into an expression without constant term and a constant.
splitConst :: Num c => Expr c -> (Expr c, c)
splitConst (Expr' ts) = (e2, c2)
  where
    p (Term _ (_:_)) = True
    p _ = False
    e2 = Expr' $ Seq.filter p ts
    c2 = sum [c | Term c [] <- toList ts]

-- | Terms.
data Term c = Term c [Var]
  deriving (Eq, Ord, Show)

instance Functor Term where
  fmap f (Term c vs) = Term (f c) vs

-- ---------------------------------------------------------------------------

-- | Objective function.
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

-- | Constraint.
--
-- In the most general case, it is of the form @x = v → L ≤ e ≤ U@.
data Constraint c
  = Constraint
  { constrLabel     :: Maybe Label
    -- ^ name of the constraint
  , constrIndicator :: Maybe (Var, c)
    -- ^ @x = v@ (v is either 0 or 1)
  , constrExpr      :: Expr c
    -- ^ expression @e@
  , constrLB        :: BoundExpr c
    -- ^ lower bound @L@
  , constrUB        :: BoundExpr c
    -- ^ upper bound @U@
  , constrIsLazy    :: Bool
    -- ^ if it is set to @True@, solver can delay adding the constraint until the constraint is violated.
  }
  deriving (Eq, Ord, Show)

-- | Lower- and Upper- bounds of a t'Constraint'.
--
-- @since 0.2.1.0
constrBounds :: Constraint c -> Bounds c
constrBounds c = (constrLB c, constrUB c)

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
    { constrIndicator = fmap (second f) (constrIndicator c)
    , constrExpr = fmap f (constrExpr c)
    , constrLB = fmap f (constrLB c)
    , constrUB = fmap f (constrUB c)
    }

-- | Relational operators.
data RelOp
  = Le  -- ^ (≤)
  | Ge  -- ^ (≥)
  | Eql -- ^ (=)
  deriving (Eq, Ord, Enum, Show)

-- ---------------------------------------------------------------------------

-- | Types of SOS (special ordered sets) constraints.
data SOSType
  = SOS1 -- ^ Type 1 SOS constraint
  | SOS2 -- ^ Type 2 SOS constraint
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- {-# DEPRECATED S1 "Use SOS1 instead" #-}
-- | Alias of 'SOS1'.
pattern S1 :: SOSType
pattern S1 = SOS1

-- {-# DEPRECATED S2 "Use SOS2 instead" #-}
-- | Alias of 'SOS2'.
pattern S2 :: SOSType
pattern S2 = SOS2

{-# COMPLETE S1, S2 #-}

-- | SOS (special ordered sets) constraints.
data SOSConstraint c
  = SOSConstraint
  { sosLabel :: Maybe Label
  , sosType  :: SOSType
  , sosBody  :: [(Var, c)]
  }
  deriving (Eq, Ord, Show)

instance Functor SOSConstraint where
  fmap f c = c{ sosBody = map (second f) (sosBody c) }

instance Default (SOSConstraint c) where
  def = SOSConstraint
        { sosLabel = Nothing
        , sosType = SOS1
        , sosBody = []
        }

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
    -- ^ status
  , solObjectiveValue :: Maybe r
    -- ^ value of the objective function
  , solVariables :: Map Var r
    -- ^ variable assignments
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

-- | Tolerance for evaluating solutions against t'Problem'.
data Tol r
  = Tol
  { integralityTol :: r
    -- ^ If a value of integer variable is within this amount from its nearest
    -- integer, it is considered feasible.
  , feasibilityTol :: r
    -- ^ If the amount of violation of constraints is within this amount, it is
    -- considered feasible.
  , optimalityTol :: r
    -- ^ Feasiblity tolerance of dual constraints.
  }
  deriving (Eq, Ord, Show)

-- | Defautl is @1e-6@ for the feasibility and optimality tolerances, and @1e-5@ for the integrality tolerance.
instance Fractional r => Default (Tol r) where
  def =
    Tol
    { integralityTol = 1e-5
    , feasibilityTol = 1e-6
    , optimalityTol = 1e-6
    }

-- | t'Tol' value with all tolerances are zero.
zeroTol :: Fractional r => Tol r
zeroTol =
  Tol
  { integralityTol = 1e-5
  , feasibilityTol = 1e-6
  , optimalityTol = 1e-6
  }

-- | Type class for evaluation various elements of t'Problem' under
-- the given variable assignments.
class Eval r a where
  -- | Result type of 'eval'
  type Evaluated r a

  -- | Evaluate a value of type @a@ under given assignments and the tolerance
  eval :: Tol r -> Map Var r -> a -> Evaluated r a

instance Num r => Eval r Var where
  type Evaluated r Var = r
  eval _tol sol v = fromMaybe 0 (Map.lookup v sol)

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
      SOS1 -> length [() | val <- body, val] <= 1
      SOS2 -> f body
    where
      body = map (not . isInBounds tol (0, 0) . eval tol sol . fst) $ sortBy (comparing snd) (sosBody sos)
      f [] = True
      f [_] = True
      f (x1 : x2 : xs)
        | x1 = all not xs
        | otherwise = f (x2 : xs)

instance (RealFrac r) => Eval r (Problem r) where
  type Evaluated r (Problem r) = Maybe r
  eval tol sol prob = do
    forM_ (Map.toList (varDomains prob)) $ \(v, dom) -> do
      guard $ isInDomain tol dom (eval tol sol v)
    forM_ (constraints prob) $ \constr -> do
      guard $ eval tol sol constr
    forM_ (sosConstraints prob) $ \constr -> do
      guard $ eval tol sol constr
    return $ eval tol sol (objectiveFunction prob)

-- | Under the given tolerance, is the value included in the domain?
--
-- @since 0.2.1.0
isInDomain :: RealFrac r => Tol r -> Domain r -> r -> Bool
isInDomain tol (vt, bounds) x = isJust $ do
  case vt of
    ContinuousVariable -> return ()
    SemiContinuousVariable -> return ()
    IntegerVariable -> guard $ isIntegral tol x
    SemiIntegerVariable -> guard $ isIntegral tol x
  case vt of
    ContinuousVariable -> guard $ isInBounds tol bounds x
    IntegerVariable -> guard $ isInBounds tol bounds x
    SemiIntegerVariable -> guard $ isInBounds tol (0,0) x || isInBounds tol bounds x
    SemiContinuousVariable -> guard $ isInBounds tol (0,0) x || isInBounds tol bounds x

-- | Under the given tolerance, is the value integral?
--
-- @since 0.2.1.0
isIntegral :: RealFrac r => Tol r -> r -> Bool
isIntegral tol x = abs (x - fromIntegral (floor (x + 0.5) :: Integer)) <= integralityTol tol

-- | Under the given tolerance, is the value within the bounds?
--
-- @since 0.2.1.0
isInBounds :: (Num r, Ord r) => Tol r -> Bounds r -> r -> Bool
isInBounds tol (lb, ub) x =
  lb - Finite (feasibilityTol tol) <= Finite x &&
  Finite x <= ub + Finite (feasibilityTol tol)

-- ---------------------------------------------------------------------------

-- | Type class for types that contain variables.
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

instance Variables Var where
  vars = Set.singleton

instance Variables (ObjectiveFunction c) where
  vars ObjectiveFunction{ objExpr = e } = vars e

instance Variables (Constraint c) where
  vars Constraint{ constrIndicator = ind, constrExpr = e } = Set.union (vars e) vs2
    where
      vs2 = maybe Set.empty (Set.singleton . fst) ind

instance Variables (SOSConstraint c) where
  vars SOSConstraint{ sosBody = xs } = Set.fromList (map fst xs)

-- ---------------------------------------------------------------------------

-- | Set of variables of a t'Problem'.
variables :: Problem c -> Set Var
variables mip = Map.keysSet $ varDomains mip

-- | Set of continuous variables of a t'Problem'.
continuousVariables :: Problem c -> Set Var
continuousVariables mip = Map.keysSet $ Map.filter ((ContinuousVariable ==) . fst) (varDomains mip)

-- | Set of integer variables of a t'Problem'.
integerVariables :: Problem c -> Set Var
integerVariables mip = Map.keysSet $ Map.filter ((IntegerVariable ==) . fst) (varDomains mip)

-- | Set of binary variables (integers variables with lower bound 0 and upper bound 1) of a t'Problem'.
binaryVariables :: (Num c, Eq c) => Problem c -> Set Var
binaryVariables mip = Map.keysSet $ Map.filter p (varDomains mip)
  where
    p (IntegerVariable, (Finite 0, Finite 1)) = True
    p (_, _) = False

-- | Set of semi-continuous variables of a t'Problem'.
semiContinuousVariables :: Problem c -> Set Var
semiContinuousVariables mip = Map.keysSet $ Map.filter ((SemiContinuousVariable ==) . fst) (varDomains mip)

-- | Set of semi-integer variables of a t'Problem'.
semiIntegerVariables :: Problem c -> Set Var
semiIntegerVariables mip = Map.keysSet $ Map.filter ((SemiIntegerVariable ==) . fst) (varDomains mip)

-- ---------------------------------------------------------------------------

-- | Options for reading/writing problem files.
data FileOptions
  = FileOptions
  { optFileEncoding :: Maybe TextEncoding
    -- ^ Text encoding used for file I/O
  , optNewline :: Maybe Newline
    -- ^ 'Newline' used for 'T.Text' data generation and writing to file.
    --
    -- If 'Nothing' is specified, 'LF' is used for text data generation
    -- assuming that newline conversion will be performed on I/O, and
    -- 'nativeNewline' is used for file writing.
    --
    -- (Default: 'Nothing')
  , optMPSWriteObjSense :: WriteSetting
    -- ^ @OBJSENSE@ section in MPS file is an extention of MPS file
    -- format for specifying the direction of the objective function
    -- in MPS file. But not all solvers support it (e.g. GLPK-4.48
    -- does not support it).
    --
    -- This option controls whether the @OBJSENSE@ sections is written.
    -- If 'WriteIfNotDefault' is used, @OBJSENSE@ is written when the
    -- objective is maximization and @OBJSENSE@ is not written
    -- when the objective is minimization.
    --
    -- (Default: 'WriteIfNotDefault')
  , optMPSWriteObjName :: Bool
    -- ^ @OBJNAME@ section is an extention of MPS file format for
    -- selecting an objective function from among the free rows within
    -- a MPS file. Not all solver support it (e.g. GLPK-4.48
    -- does not support it).
    --
    -- This option controls whether the @OBJNAME@ section is written.
    --
    -- (Default: 'True')
  } deriving (Show)

instance Default FileOptions where
  def =
    FileOptions
    { optFileEncoding = Nothing
    , optNewline = Nothing
    , optMPSWriteObjSense = WriteIfNotDefault
    , optMPSWriteObjName = True
    }

-- | Options for writing a particular data to files.
data WriteSetting
  = WriteAlways
    -- ^ Always write the data.
  | WriteIfNotDefault
    -- ^ Write the data only if it is not the default value.
  | WriteNever
    -- ^ Never write the data.
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- | Checks if all variable names and labels in the problem are ASCII.
isAscii :: Problem c -> Bool
isAscii prob = and
  [ all p $ catMaybes [name prob, objLabel (objectiveFunction prob)]
  , all p $ catMaybes $ map constrLabel $ constraints prob
  , all p $ catMaybes $ map constrLabel $ userCuts prob
  , all p $ catMaybes $ map sosLabel $ sosConstraints prob
  , all (p . varName) $ Map.keys (varDomains prob)
  ]
  where
#if MIN_VERSION_text(2,0,2)
    p = T.isAscii
#else
    p = T.all Char.isAscii
#endif
