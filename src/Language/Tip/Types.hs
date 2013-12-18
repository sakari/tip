{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Language.Tip.Types (infer) where

import Data.Generics
import Language.Tip.Symbols
import qualified Language.Tip.Ast as Ast
import Text.Parsec.Pos (newPos, SourcePos, sourceColumn, sourceName, sourceLine)

import Language.Tip.Scope
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State.Strict
import Control.Applicative
import Data.Maybe
import Data.List hiding (minimum)
import Prelude hiding (minimum)

type Typed = Either [String] ()

infer :: Ast.Module -> Typed
infer Ast.Module { Ast.moduleStatements } = result
    where
      result = case errors $ runTyping typechecked of
                 [] -> Right ()
                 es -> Left $ map pretty es
      typechecked = scope vars $ mapM_ typeStmt moduleStatements
      vars = Set.toList $ declarations moduleStatements

pretty :: TypingError -> String
pretty NotSubtype { sub, super, at } = concat $ intersperse " " [
                                        prettyType sub, "does not fit"
                                       , prettyType super,  "at"
                                       , position ]
    where
      position = sourceName at ++ ":" ++ show (sourceLine at) ++ " column " ++ show (sourceColumn at)

prettyType = go
    where
      go StringType = "string"
      go NumberType = "number"

data Constraint = Constraint { constraintPosition :: SourcePos, constraintType :: Ty }
                deriving (Show)

data Ty = StringType
        | NumberType
        | Structure { structure :: Map.Map String Ty }
        | Function { parameters :: [Ty], returnType :: Ty }
        | Reference { ref :: Ref }
        | ConstrainedType { constraints :: [Constraint]
                          , minimum :: Maybe Ty }
          deriving (Show)

data TypingError = NotSubtype { sub :: Ty,  super :: Ty, at :: SourcePos }
                 deriving (Show)

data Env = Env { errors :: [TypingError]
               , env :: Map.Map Ref Ty
               , position :: SourcePos }

emptyEnv = Env { errors = [], env = Map.empty, position = newPos "" 0 0  }

newtype TypingM a = TypingM { typingM :: ScopeT (State Env) a }
    deriving (Monad, Functor, ScopeMonad)

instance MonadState Env TypingM where
    get = TypingM $ lift $ get
    put s = TypingM $ lift $ put s

runTyping :: TypingM a -> Env
runTyping t = snd $ ( runState $ runScope $ typingM t) emptyEnv

currentPosition :: TypingM SourcePos
currentPosition = do
  gets position

atPosition :: SourcePos -> TypingM a -> TypingM a
atPosition p m = do
  previous <- currentPosition
  modify $ w p
  a <- m
  modify $ w previous
  return a
    where
      w p e = e { position = p }

typeStmt :: Ast.Statement -> TypingM ()
typeStmt Ast.Statement { Ast.stmt = Ast.ExpressionStmt { Ast.expression }} =
    typeExpression expression >> return ()
typeStmt Ast.Statement { Ast.stmt = Ast.VarStmt {}} = return ()
typeStmt stmt = error $ "missing typeStmt case for: " ++ show stmt

typeExpression e@Ast.Expression { Ast.expr, Ast.exprType } = atPosition pos $
                                                             go expr >>= typed exprType
    where
      pos = Ast.getPosition e
      typed Nothing x_type = return x_type
      typed (Just t) x_type = do
        assignedType <- typeLiteral t
        (x_type', assigned') <- x_type |> assignedType
        return assigned'

      go Ast.Identifier { Ast.identifierName } = do
        resolved <- resolve identifierName
        case resolved of
          Nothing -> error $ "could not resolve identifier: " ++ identifierName
          Just i -> return $ Reference i

      go Ast.StringLiteral {} = return StringType
      go Ast.IntLiteral {} = return NumberType
      go Ast.Assignment { Ast.lhs, Ast.rhs } = do
        lhs_t <- typeExpression lhs
        rhs_t <- typeExpression rhs
        lhs_t |> rhs_t
        return lhs_t

      go Ast.Application { Ast.callee, Ast.arguments } = do
        t <- typeExpression callee
        args <- mapM typeExpression arguments
        r <- fresh
        let ref = Reference { ref = r }
            c = Function { parameters = args, returnType = ref }
        c |> t
        return ref
      go x = error $ "typeExpression missing case for: " ++ show x

typeLiteral :: Ast.Type -> TypingM Ty
typeLiteral t = go t
    where
      go Ast.NumberType = return NumberType
      go Ast.StringType = return StringType
      go t = error $ "missing typeliteral case: " ++ show t

typeof :: Ref -> TypingM Ty
typeof r = do
  env <- gets env
  case Map.lookup r env of
    Nothing -> asserttype r $ ConstrainedType { minimum = Nothing, constraints = []}
    Just i -> return i

asserttype :: Ref -> Ty -> TypingM Ty
asserttype r t = modify w >> return t
    where
      w e@Env { env } = e { env = Map.insert r t env }

mismatch :: Ty -> Ty -> TypingM ()
mismatch l r = modify w
    where
      w e@Env { errors, position } = e { errors = k position:errors }
      k p = NotSubtype { super = l, sub = r, at = p }

(|>) :: Ty -> Ty -> TypingM (Ty, Ty)
l |> r = go l r
    where
      go var@Reference { ref } rhs = do
        refType <- typeof ref
        (l', r') <- refType |> r
        asserttype ref l'
        return (var, r')

      go lhs var@Reference { ref } = do
        refType <- typeof ref
        (l', r') <- l |> refType
        asserttype ref $ r'
        return (l', var)

      go StringType StringType = return (StringType, StringType)
      go NumberType NumberType = return (NumberType, NumberType)
      go StringType NumberType = mismatch l r >> return (StringType, NumberType)
      go NumberType StringType = mismatch l r >> return (StringType, NumberType)

      go c@ConstrainedType { minimum = Nothing, constraints } _ = do
        r_c <- constraint r
        return (c { constraints = r_c : constraints }, r)

      go c@ConstrainedType { minimum = Just t, constraints } _ = do
        (l', r') <- t |> r
        r_c <- constraint r'
        let c_new = c { minimum = Just l', constraints = r_c:constraints }
        solve c_new
        return (c_new, r')

      go l c@ConstrainedType { minimum = Nothing, constraints } = do
        solve c_new
        return (l, c_new)
          where
            c_new = c { minimum = Just l }

      go _ c@ConstrainedType { constraints, minimum = Just t } = do
        (l', t') <- l |> t
        let c_new = c { minimum = Just t' }
        solve c_new
        return (l', c_new)

      go lhs rhs = mismatch l r >> return (lhs, rhs)

constraint :: Ty -> TypingM Constraint
constraint t = do
  p <- currentPosition
  return $ Constraint { constraintPosition = p, constraintType = t}

solve :: Ty -> TypingM ()
solve t = go t
    where
      w m Constraint { constraintPosition, constraintType } =
          atPosition constraintPosition $
                     m |> constraintType
      go ConstrainedType { minimum = Just m, constraints } =
          mapM_ (w m) constraints
      go _ = return ()
