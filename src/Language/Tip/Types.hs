{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Language.Tip.Types (infer) where

import Data.Generics
import Language.Tip.Symbols
import qualified Language.Tip.Ast as Ast
import Language.Tip.Scope
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State.Strict
import Control.Applicative
import Data.Maybe
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
pretty NotSubtype { sub, super } = p sub ++ " is not a subtype of " ++ p super
    where
      p o = "'" ++ (prettyType o) ++ "' at " ++ prettyPos o

prettyType = go . ty
    where
      go StringType = "string"
      go NumberType = "number"

prettyPos o = Ast.srcName pos ++ ":" ++ show (Ast.srcLine pos)
    where
      pos = Ast.getPosition o

instance Ast.Positioned Type where
    getPosition ExpressionType { expr } = Ast.getPosition expr
    getPosition StmtType { stmt } = Ast.getPosition stmt

data Type = ExpressionType { expr :: Ast.Expression
                           , ty :: Ty}
          | StmtType { stmt :: Ast.Statement
                     , ty :: Ty }
            deriving (Show)

data Ty = StringType
        | NumberType
        | Structure { structure :: Map.Map String Type }
        | Function { parameters :: [Type], returnType :: Type }
        | Reference { ref :: Ref }
        | ConstrainedType { constraints :: [Type]
                          , minimum :: Maybe Ty }
          deriving (Show)

data TypingError = NotSubtype { sub :: Type,  super :: Type }
                 deriving (Show)

data Env = Env { errors :: [TypingError]
               , env :: Map.Map Ref Ty }

emptyEnv = Env { errors = [], env = Map.empty }

newtype TypingM a = TypingM { typingM :: ScopeT (State Env) a }
    deriving (Monad, Functor, ScopeMonad)

instance MonadState Env TypingM where
    get = TypingM $ lift $ get
    put s = TypingM $ lift $ put s

runTyping :: TypingM a -> Env
runTyping t = snd $ ( runState $ runScope $ typingM t) emptyEnv

typeStmt :: Ast.Statement -> TypingM ()
typeStmt Ast.Statement { Ast.stmt = Ast.ExpressionStmt { Ast.expression }} =
    typeExpression expression >> return ()
typeStmt Ast.Statement { Ast.stmt = Ast.VarStmt {}} = return ()
typeStmt stmt = error $ "missing typeStmt case for: " ++ show stmt

typeExpression e@Ast.Expression { Ast.expr, Ast.exprType } = go expr >>= typed exprType
    where
      typed Nothing x_type = return x_type
      typed (Just t) x_type = do
        assignedType <- typeLiteral t
        (x_type', assigned') <- x_type |> x_type { ty = assignedType }
        return assigned'

      go Ast.Identifier { Ast.identifierName } = do
        resolved <- resolve identifierName
        case resolved of
          Nothing -> error $ "could not resolve identifier: " ++ identifierName
          Just i -> return $ ExpressionType e $ Reference i

      go Ast.StringLiteral {} = return $ ExpressionType e StringType
      go Ast.IntLiteral {} = return $ ExpressionType e NumberType
      go Ast.Assignment { Ast.lhs, Ast.rhs } = do
        lhs_t <- typeExpression lhs
        rhs_t <- typeExpression rhs
        lhs_t |> rhs_t
        return lhs_t

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

mismatch :: Type -> Type -> TypingM ()
mismatch l r = modify w
    where
      w e@Env { errors } = e { errors = k:errors }
      k = NotSubtype { super = l, sub = r }

(|>) :: Type -> Type -> TypingM (Type, Type)
l |> r = do
  (ty_l, ty_r) <- go (ty l) (ty r)
  return (l { ty = ty_l }, r { ty = ty_r })
    where
      go var@Reference { ref } rhs = do
        refType <- typeof ref
        (l', r') <- l { ty = refType } |> r
        asserttype ref $ ty l'
        return (var, ty r')

      go lhs var@Reference { ref } = do
        refType <- typeof ref
        (l', r') <- l |> r { ty = refType }
        asserttype ref $ ty r'
        return (ty l', var)

      go StringType StringType = return (StringType, StringType)
      go NumberType NumberType = return (NumberType, NumberType)
      go StringType NumberType = mismatch l r >> return (StringType, NumberType)
      go NumberType StringType = mismatch l r >> return (StringType, NumberType)

      go c@ConstrainedType { minimum = Nothing, constraints } _ = return (c_new, ty r)
          where
            c_new = c { constraints = r:constraints }

      go c@ConstrainedType { minimum = Just t, constraints } _ = do
        (l', r') <- l { ty = t }  |> r
        let c_new = c { minimum = Just $ ty l', constraints = r':constraints }
        solve $ l { ty = c_new }
        return (c_new, ty r')

      go l c@ConstrainedType { minimum = Nothing, constraints } = do
        solve $ r { ty = c_new }
        return (l, c_new)
          where
            c_new = c { minimum = Just l }

      go _ c@ConstrainedType { constraints, minimum = Just t } = do
        (l', t') <- l |> r { ty = t }
        let c_new = c { minimum = Just $ ty t' }
        solve $ r { ty = c_new }
        return (ty l', c_new)

      go lhs rhs = mismatch l r >> return (lhs, rhs)

solve :: Type -> TypingM ()
solve t = go $ ty t
    where
      go ConstrainedType { minimum = Just m, constraints } =
          mapM_ (t { ty = m }  |>) constraints
      go _ = return ()
