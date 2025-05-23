{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.MIP.LPFile
-- Copyright   :  (c) Masahiro Sakai 2011-2014
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-- A CPLEX @.lp@ format parser library.
--
-- References:
--
-- * <http://publib.boulder.ibm.com/infocenter/cosinfoc/v12r2/index.jsp?topic=/ilog.odms.cplex.help/Content/Optimization/Documentation/CPLEX/_pubskel/CPLEX880.html>
--
-- * <http://www.gurobi.com/doc/45/refman/node589.html>
--
-- * <http://lpsolve.sourceforge.net/5.5/CPLEX-format.htm>
--
-----------------------------------------------------------------------------
module Numeric.Optimization.MIP.LPFile
  ( parseString
  , parseFile
  , ParseError
  , parser
  , render
  ) where

import Control.Applicative hiding (many)
import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.Writer
import Control.Monad.ST
import Data.Char
import Data.Default.Class
import Data.Either (lefts, rights)
import Data.List
import Data.Maybe
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.STRef
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.Scientific as B
import Data.OptDir
import System.IO
import Text.Megaparsec hiding (label, skipManyTill, ParseError)
import Text.Megaparsec.Char hiding (string', char', newline)
import qualified Text.Megaparsec.Char.Lexer as P

import qualified Numeric.Optimization.MIP.Base as MIP
import Numeric.Optimization.MIP.FileUtils (ParseError, readTextFile)
import Numeric.Optimization.MIP.Internal.Util (combineMaybe)

-- ---------------------------------------------------------------------------

type C e s m = (MonadParsec e s m, Token s ~ Char, IsString (Tokens s))

-- | Parse a string containing LP file data.
--
-- The source name is only used in error messages and may be the empty string.
parseString :: (Stream s, Token s ~ Char, IsString (Tokens s)) => MIP.FileOptions -> String -> s -> Either (ParseError s) (MIP.Problem Scientific)
parseString _ = parse (parser <* eof)

-- | Parse a file containing LP file data.
parseFile :: MIP.FileOptions -> FilePath -> IO (MIP.Problem Scientific)
parseFile opt fname = do
  s <- readTextFile opt fname
  case parse (parser <* eof) fname s of
    Left e -> throwIO (e :: ParseError TL.Text)
    Right a -> return a

-- ---------------------------------------------------------------------------

anyChar :: C e s m => m Char
anyChar = anySingle

char' :: C e s m => Char -> m Char
char' c = (char c <|> char (toUpper c)) <?> show c

string' :: C e s m => String -> m ()
string' s = mapM_ char' s <?> show s

sep :: C e s m => m ()
sep = skipMany (void comment <|> void spaceChar)

comment :: C e s m => m ()
comment = do
  char '\\'
  skipManyTill anyChar (try eol)

tok :: C e s m => m a -> m a
tok p = do
  x <- p
  sep
  return x

ident :: C e s m => m String
ident = tok $ do
  x <- letterChar <|> oneOf syms1
  xs <- many (alphaNumChar <|> oneOf syms2)
  let s = x:xs
  guard $ map toLower s `Set.notMember` reserved
  return s
  where
    syms1 = "!\"#$%&()/,;?@_`'{}|~"
    syms2 = '.' : syms1

variable :: C e s m => m MIP.Var
variable = liftM fromString ident

label :: C e s m => m MIP.Label
label = do
  name <- ident
  tok $ char ':'
  return $! T.pack name

reserved :: Set String
reserved = Set.fromList
  [ "bound", "bounds"
  , "gen", "general", "generals"
  , "bin", "binary", "binaries"
  , "semi", "semi-continuous", "semis"
  , "sos"
  , "end"
  , "subject"
  ]

-- ---------------------------------------------------------------------------

-- | LP file parser
parser :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m (MIP.Problem Scientific)
parser = do
  name <- optional $ try $ do
    space
    string' "\\* Problem: "
    liftM fromString $ manyTill anyChar (try (string " *\\" >> eol))
  sep
  obj <- problem

  cs <- liftM concat $ many $ msum $
    [ liftM (map Left) constraintSection
    , liftM (map Left) lazyConstraintsSection
    , liftM (map Right) userCutsSection
    ]

  bnds <- option Map.empty (try boundsSection)
  exvs <- many (liftM Left generalSection <|> liftM Right binarySection)
  let ints = Set.fromList $ concat (lefts exvs)
      bins = Set.fromList $ concat (rights exvs)
  bnds2 <- return $ Map.unionWith MIP.intersectBounds
            bnds (Map.fromAscList [(v, (MIP.Finite 0, MIP.Finite 1)) | v <- Set.toAscList bins])
  scs <- liftM Set.fromList $ option [] (try semiSection)

  ss <- option [] (try sosSection)
  end
  let vs = Set.unions $ map MIP.vars cs ++
           [ Map.keysSet bnds2
           , ints
           , bins
           , scs
           , MIP.vars obj
           , MIP.vars ss
           ]
      isInt v  = v `Set.member` ints || v `Set.member` bins
      isSemi v = v `Set.member` scs
  return $
    MIP.Problem
    { MIP.name              = name
    , MIP.objectiveFunction = obj
    , MIP.constraints       = lefts cs
    , MIP.userCuts          = rights cs
    , MIP.sosConstraints    = ss
    , MIP.varDomains        = Map.fromAscList
       [ (v, (t, bs))
       | v <- Set.toAscList vs
       , let t =
               if isInt v then
                 if isSemi v then MIP.SemiIntegerVariable
                 else MIP.IntegerVariable
               else
                 if isSemi v then MIP.SemiContinuousVariable
                 else MIP.ContinuousVariable
       , let bs = Map.findWithDefault MIP.defaultBounds v bnds2
       ]
    }

problem :: C e s m => m (MIP.ObjectiveFunction Scientific)
problem = do
  flag <-  (try minimize >> return OptMin)
       <|> (try maximize >> return OptMax)
  name <- optional (try label)
  obj <- expr
  return def{ MIP.objLabel = name, MIP.objDir = flag, MIP.objExpr = obj }

minimize, maximize :: C e s m => m ()
minimize = tok $ string' "min" >> optional (string' "imize") >> return ()
maximize = tok $ string' "max" >> optional (string' "imize") >> return ()

end :: C e s m => m ()
end = tok $ string' "end"

-- ---------------------------------------------------------------------------

constraintSection :: C e s m => m [MIP.Constraint Scientific]
constraintSection = subjectTo >> many (try (constraint False))

subjectTo :: C e s m => m ()
subjectTo = msum
  [ try $ tok (string' "subject") >> tok (string' "to")
  , try $ tok (string' "such") >> tok (string' "that")
  , try $ tok (string' "st")
  , try $ tok (string' "s") >> optional (tok (char '.')) >> tok (string' "t")
        >> tok (char '.') >> return ()
  ]

constraint :: C e s m => Bool -> m (MIP.Constraint Scientific)
constraint isLazy = do
  name <- optional (try label)
  g <- optional $ try indicator

  -- It seems that CPLEX allows empty lhs, but GLPK rejects it.
  e <- expr
  op <- relOp
  s <- option 1 sign
  rhs <- liftM (s*) number

  let (lb,ub) =
        case op of
          MIP.Le -> (MIP.NegInf, MIP.Finite rhs)
          MIP.Ge -> (MIP.Finite rhs, MIP.PosInf)
          MIP.Eql -> (MIP.Finite rhs, MIP.Finite rhs)

  return $ MIP.Constraint
    { MIP.constrLabel     = name
    , MIP.constrIndicator = g
    , MIP.constrExpr      = e
    , MIP.constrLB        = lb
    , MIP.constrUB        = ub
    , MIP.constrIsLazy    = isLazy
    }

relOp :: C e s m => m MIP.RelOp
relOp = tok $ msum
  [ char '<' >> optional (char '=') >> return MIP.Le
  , char '>' >> optional (char '=') >> return MIP.Ge
  , char '=' >> msum [ char '<' >> return MIP.Le
                     , char '>' >> return MIP.Ge
                     , return MIP.Eql
                     ]
  ]

indicator :: C e s m => m (MIP.Var, Scientific)
indicator = do
  var <- variable
  tok (char '=')
  val <- number  -- numbers other than 0 or 1 should be error?
  tok $ string "->"
  return (var, val)

lazyConstraintsSection :: C e s m => m [MIP.Constraint Scientific]
lazyConstraintsSection = do
  tok $ string' "lazy"
  tok $ string' "constraints"
  many $ try $ constraint True

userCutsSection :: C e s m => m [MIP.Constraint Scientific]
userCutsSection = do
  tok $ string' "user"
  tok $ string' "cuts"
  many $ try $ constraint False

type Bounds2 c = (Maybe (MIP.BoundExpr c), Maybe (MIP.BoundExpr c))

boundsSection :: C e s m => m (Map MIP.Var (MIP.Bounds Scientific))
boundsSection = do
  tok $ string' "bound" >> optional (char' 's')
  liftM (Map.map g . Map.fromListWith f) $ many (try bound)
  where
    f (lb1,ub1) (lb2,ub2) = (combineMaybe max lb1 lb2, combineMaybe min ub1 ub2)
    g (lb, ub) = ( fromMaybe MIP.defaultLB lb
                 , fromMaybe MIP.defaultUB ub
                 )

bound :: C e s m => m (MIP.Var, Bounds2 Scientific)
bound = msum
  [ try $ do
      v <- try variable
      msum
        [ do
            op <- relOp
            b <- boundExpr
            return
              ( v
              , case op of
                  MIP.Le -> (Nothing, Just b)
                  MIP.Ge -> (Just b, Nothing)
                  MIP.Eql -> (Just b, Just b)
              )
        , do
            tok $ string' "free"
            return (v, (Just MIP.NegInf, Just MIP.PosInf))
        ]
  , do
      b1 <- liftM Just boundExpr
      op1 <- relOp
      guard $ op1 == MIP.Le
      v <- variable
      b2 <- option Nothing $ do
        op2 <- relOp
        guard $ op2 == MIP.Le
        liftM Just boundExpr
      return (v, (b1, b2))
  ]

boundExpr :: C e s m => m (MIP.BoundExpr Scientific)
boundExpr = msum
  [ try (tok (char '+') >> inf >> return MIP.PosInf)
  , try (tok (char '-') >> inf >> return MIP.NegInf)
  , do
      s <- option 1 sign
      x <- number
      return $ MIP.Finite (s*x)
  ]

inf :: C e s m => m ()
inf = void (tok (string "inf" >> optional (string "inity")))

-- ---------------------------------------------------------------------------

generalSection :: C e s m => m [MIP.Var]
generalSection = do
  tok $ string' "gen" >> optional (string' "eral" >> optional (string' "s"))
  many (try variable)

binarySection :: C e s m => m [MIP.Var]
binarySection = do
  tok $ string' "bin" >> optional (string' "ar" >> (string' "y" <|> string' "ies"))
  many (try variable)

semiSection :: C e s m => m [MIP.Var]
semiSection = do
  tok $ string' "semi" >> optional (string' "-continuous" <|> string' "s")
  many (try variable)

sosSection :: C e s m => m [MIP.SOSConstraint Scientific]
sosSection = do
  tok $ string' "sos"
  many $ try $ do
    (l,t) <- try (do { l <- label; t <- typ; return (Just l, t) })
          <|> (do { t <- typ; return (Nothing, t) })
    xs <- many $ try $ do
      v <- variable
      tok $ char ':'
      w <- number
      return (v,w)
    return $ MIP.SOSConstraint l t xs
  where
    typ = do
      t <- tok $ (char' 's' >> ((char '1' >> return MIP.SOS1) <|> (char '2' >> return MIP.SOS2)))
      tok (string "::")
      return t

-- ---------------------------------------------------------------------------

expr :: forall e s m. C e s m => m (MIP.Expr Scientific)
expr = try expr1 <|> return 0
  where
    expr1 :: m (MIP.Expr Scientific)
    expr1 = do
      t <- term True
      ts <- many (term False)
      return $ foldr (+) 0 (t : ts)

sign :: (C e s m, Num a) => m a
sign = tok ((char '+' >> return 1) <|> (char '-' >> return (-1)))

term :: C e s m => Bool -> m (MIP.Expr Scientific)
term flag = do
  s <- if flag then optional sign else liftM Just sign
  c <- optional number
  e <- liftM MIP.varExpr variable <|> qexpr
  return $ case combineMaybe (*) s c of
    Nothing -> e
    Just d -> MIP.constExpr d * e

qexpr :: C e s m => m (MIP.Expr Scientific)
qexpr = do
  tok (char '[')
  t <- qterm True
  ts <- many (qterm False)
  let e = MIP.Expr (t:ts)
  tok (char ']')
  -- Gurobi allows ommiting "/2"
  (do mapM_ (tok . char) ("/2" :: String) -- Explicit type signature is necessary because the type of mapM_ in GHC-7.10 is generalized for arbitrary Foldable
      return $ MIP.constExpr (1/2) * e)
   <|> return e

qterm :: C e s m => Bool -> m (MIP.Term Scientific)
qterm flag = do
  s <- if flag then optional sign else liftM Just sign
  c <- optional number
  es <- do
    e <- qfactor
    es <- many (tok (char '*') >> qfactor)
    return $ e ++ concat es
  return $ case combineMaybe (*) s c of
    Nothing -> MIP.Term 1 es
    Just d -> MIP.Term d es

qfactor :: C e s m => m [MIP.Var]
qfactor = do
  v <- variable
  msum [ tok (char '^') >> tok (char '2') >> return [v,v]
       , return [v]
       ]

number :: forall e s m. C e s m => m Scientific
number = tok $ P.signed sep P.scientific

skipManyTill :: Alternative m => m a -> m end -> m ()
skipManyTill p end' = scan
  where
    scan = (end' *> pure ()) <|> (p *> scan)

-- ---------------------------------------------------------------------------

type M a = Writer Builder a

execM :: M a -> TL.Text
execM m = B.toLazyText $ execWriter m

writeString :: T.Text -> M ()
writeString s = tell $ B.fromText s

writeChar :: Char -> M ()
writeChar c = tell $ B.singleton c

-- ---------------------------------------------------------------------------

-- | Render a problem into a 'TL.Text' containing LP file data.
render :: MIP.FileOptions -> MIP.Problem Scientific -> Either String TL.Text
render opt mip = Right $ execM $ render' opt $ normalize mip

writeVar :: MIP.Var -> M ()
writeVar (MIP.Var v) = writeString v

render' :: MIP.FileOptions -> MIP.Problem Scientific -> M ()
render' opt mip = do
  let newline =
        case fromMaybe LF (MIP.optNewline opt) of
          LF -> "\n"
          CRLF -> "\r\n"
      writeStringLn s = do
        writeString s
        writeString newline

  case MIP.name mip of
    Just name -> do
      writeString $ "\\* Problem: " <> name <> " *\\"
      writeString newline
    Nothing -> return ()

  let obj = MIP.objectiveFunction mip

  writeString $
    case MIP.objDir obj of
      OptMin -> "MINIMIZE"
      OptMax -> "MAXIMIZE"
  writeString newline

  renderLabel (MIP.objLabel obj)
  renderExpr newline True (MIP.objExpr obj)
  writeString newline

  writeStringLn "SUBJECT TO"
  forM_ (MIP.constraints mip) $ \c -> do
    unless (MIP.constrIsLazy c) $ do
      renderConstraint newline c
      writeString newline

  let lcs = [c | c <- MIP.constraints mip, MIP.constrIsLazy c]
  unless (null lcs) $ do
    writeStringLn "LAZY CONSTRAINTS"
    forM_ lcs $ \c -> do
      renderConstraint newline c
      writeString newline

  let cuts = MIP.userCuts mip
  unless (null cuts) $ do
    writeStringLn "USER CUTS"
    forM_ cuts $ \c -> do
      renderConstraint newline c
      writeString newline

  let ivs = MIP.integerVariables mip `Set.union` MIP.semiIntegerVariables mip
      (bins,gens) = Set.partition (\v -> MIP.getBounds mip v == (MIP.Finite 0, MIP.Finite 1)) ivs
      scs = MIP.semiContinuousVariables mip `Set.union` MIP.semiIntegerVariables mip

  writeStringLn "BOUNDS"
  forM_ (Map.toAscList (MIP.varBounds mip)) $ \(v, (lb,ub)) -> do
    unless (v `Set.member` bins) $ do
      renderBoundExpr lb
      writeString " <= "
      writeVar v
      writeString " <= "
      renderBoundExpr ub
      writeString newline

  unless (Set.null gens) $ do
    writeStringLn "GENERALS"
    renderVariableList newline $ Set.toList gens

  unless (Set.null bins) $ do
    writeStringLn "BINARIES"
    renderVariableList newline $ Set.toList bins

  unless (Set.null scs) $ do
    writeStringLn "SEMI-CONTINUOUS"
    renderVariableList newline $ Set.toList scs

  unless (null (MIP.sosConstraints mip)) $ do
    writeStringLn "SOS"
    forM_ (MIP.sosConstraints mip) $ \(MIP.SOSConstraint l typ xs) -> do
      renderLabel l
      writeString $ case typ of
        MIP.SOS1 -> "S1"
        MIP.SOS2 -> "S2"
      writeString " ::"
      forM_ xs $ \(v, r) -> do
        writeString "  "
        writeVar v
        writeString " : "
        tell $ B.scientificBuilder r
      writeString newline

  writeStringLn "END"

-- FIXME: Gurobi は quadratic term が最後に一つある形式でないとダメっぽい
renderExpr :: T.Text -> Bool -> MIP.Expr Scientific -> M ()
renderExpr newline isObj e = fill newline 80 (ts1 ++ ts2)
  where
    (ts,qts) = partition isLin (MIP.terms e)
    isLin (MIP.Term _ [])  = True
    isLin (MIP.Term _ [_]) = True
    isLin _ = False

    ts1 = map f ts
    ts2
      | null qts  = []
      | otherwise =
        -- マイナスで始めるとSCIP 2.1.1 は「cannot have '-' in front of quadratic part ('[')」というエラーを出す
        -- SCIP-3.1.0 does not allow spaces between '/' and '2'.
        ["+ ["] ++ map g qts ++ [if isObj then "] /2" else "]"]

    f :: MIP.Term Scientific -> T.Text
    f (MIP.Term c [])  = showConstTerm c
    f (MIP.Term c [v]) = showCoeff c <> MIP.varName v
    f _ = error "should not happen"

    g :: MIP.Term Scientific -> T.Text
    g (MIP.Term c vs) =
      (if isObj then showCoeff (2*c) else showCoeff c) <>
      mconcat (intersperse " * " (map MIP.varName vs))

showValue :: Scientific -> T.Text
showValue = fromString . show

showCoeff :: Scientific -> T.Text
showCoeff c =
  if c' == 1
    then s
    else s <> showValue c' <> " "
  where
    c' = abs c
    s = if c >= 0 then "+ " else "- "

showConstTerm :: Scientific -> T.Text
showConstTerm c = s <> showValue (abs c)
  where
    s = if c >= 0 then "+ " else "- "

renderLabel :: Maybe MIP.Label -> M ()
renderLabel l =
  case l of
    Nothing -> return ()
    Just s -> writeString s >> writeString ": "

renderOp :: MIP.RelOp -> M ()
renderOp MIP.Le = writeString "<="
renderOp MIP.Ge = writeString ">="
renderOp MIP.Eql = writeString "="

renderConstraint :: T.Text -> MIP.Constraint Scientific -> M ()
renderConstraint newline c@MIP.Constraint{ MIP.constrExpr = e, MIP.constrLB = lb, MIP.constrUB = ub } = do
  renderLabel (MIP.constrLabel c)
  case MIP.constrIndicator c of
    Nothing -> return ()
    Just (v,vval) -> do
      writeVar v
      writeString " = "
      tell $
        case floatingOrInteger vval of
          Right (i :: Integer) -> B.decimal i
          Left (_ :: Double) -> B.scientificBuilder vval  -- should be error?
      writeString " -> "

  renderExpr newline False e
  writeChar ' '
  let (op, val) =
        case (lb, ub) of
          (MIP.NegInf, MIP.Finite x) -> (MIP.Le, x)
          (MIP.Finite x, MIP.PosInf) -> (MIP.Ge, x)
          (MIP.Finite x1, MIP.Finite x2) | x1==x2 -> (MIP.Eql, x1)
          _ -> error "Numeric.Optimization.MIP.LPFile.renderConstraint: should not happen"
  renderOp op
  writeChar ' '
  tell $ B.scientificBuilder val

renderBoundExpr :: MIP.BoundExpr Scientific -> M ()
renderBoundExpr (MIP.Finite r) = tell $ B.scientificBuilder r
renderBoundExpr MIP.NegInf = writeString "-inf"
renderBoundExpr MIP.PosInf = writeString "+inf"

renderVariableList :: T.Text -> [MIP.Var] -> M ()
renderVariableList newline vs = fill newline 80 (map MIP.varName vs) >> writeString newline

fill :: T.Text -> Int -> [T.Text] -> M ()
fill newline width str = go str 0
  where
    go [] _ = return ()
    go (x:xs) 0 = writeString x >> go xs (T.length x)
    go (x:xs) w =
      if w + 1 + T.length x <= width
        then writeChar ' ' >> writeString x >> go xs (w + 1 + T.length x)
        else writeString newline >> go (x:xs) 0

-- ---------------------------------------------------------------------------

{-
compileExpr :: Expr -> Maybe (Map Var Scientific)
compileExpr e = do
  xs <- forM e $ \(Term c vs) ->
    case vs of
      [v] -> return (v, c)
      _ -> mzero
  return (Map.fromList xs)
-}

-- ---------------------------------------------------------------------------

normalize :: (Eq r, Num r) => MIP.Problem r -> MIP.Problem r
normalize = removeEmptyExpr . removeRangeConstraints

removeRangeConstraints :: (Eq r, Num r) => MIP.Problem r -> MIP.Problem r
removeRangeConstraints prob = runST $ do
  vsRef <- newSTRef $ MIP.variables prob
  cntRef <- newSTRef (0::Int)
  newvsRef <- newSTRef []

  let gensym = do
        vs <- readSTRef vsRef
        let loop !c = do
              let v = fromString ("~r_" ++ show c)
              if v `Set.member` vs then
                loop (c+1)
              else do
                writeSTRef cntRef $! c+1
                modifySTRef vsRef (Set.insert v)
                return v
        loop =<< readSTRef cntRef

  cs2 <- forM (MIP.constraints prob) $ \c -> do
    case (MIP.constrLB c, MIP.constrUB c) of
      (MIP.NegInf, MIP.Finite _) -> return c
      (MIP.Finite _, MIP.PosInf) -> return c
      (MIP.Finite x1, MIP.Finite x2) | x1 == x2 -> return c
      (lb, ub) -> do
        v <- gensym
        modifySTRef newvsRef ((v, (lb,ub)) :)
        return $
          c
          { MIP.constrExpr = MIP.constrExpr c - MIP.varExpr v
          , MIP.constrLB = MIP.Finite 0
          , MIP.constrUB = MIP.Finite 0
          }

  newvs <- liftM reverse $ readSTRef newvsRef
  return $
    prob
    { MIP.constraints = cs2
    , MIP.varDomains = MIP.varDomains prob `Map.union` Map.fromList [(v, (MIP.ContinuousVariable, bs)) | (v,bs) <- newvs]
    }

removeEmptyExpr :: Num r => MIP.Problem r -> MIP.Problem r
removeEmptyExpr prob =
  prob
  { MIP.objectiveFunction = obj{ MIP.objExpr = convertExpr (MIP.objExpr obj) }
  , MIP.constraints = map convertConstr $ MIP.constraints prob
  , MIP.userCuts    = map convertConstr $ MIP.userCuts prob
  }
  where
    obj = MIP.objectiveFunction prob

    convertExpr (MIP.Expr []) = MIP.Expr [MIP.Term 0 [fromString "x0"]]
    convertExpr e = e

    convertConstr constr =
      constr
      { MIP.constrExpr = convertExpr $ MIP.constrExpr constr
      }
