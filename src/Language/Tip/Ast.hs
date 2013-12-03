{-# LANGUAGE DeriveDataTypeable #-}
module Language.Tip.Ast where

import Data.Typeable
import Data.Data
import Text.Parsec hiding (getPosition)

data Module = Module {
      moduleFilePath :: FilePath
    , moduleStatements :: [Statement]
    } deriving (Show, Data, Typeable)

data Statement = Statement {
      statementPosition :: SourcePos
    , stmt :: Stmt
    } deriving (Show, Data, Typeable)

data Stmt = ExpressionStmt { expression :: Expression }
          | Async { resultList :: [Id], asyncCall :: Expression }
          | ReturnStmt { returnExpression :: Maybe Expression }
          | YieldStmt { yield :: [Expression] }
          | IfStmt { condition :: Expression
                   , ifBody :: [Statement]
                   , elseBranch:: [Statement] }
            deriving (Show, Data, Typeable)

data Id = Id { idName :: String }
        | IdQuote { idQuote :: String }
          deriving (Show, Data, Typeable)

data Expression = Expression {
      expressionPosition :: SourcePos
    , expr :: Expr
    } deriving (Show, Data, Typeable)

data Expr = Identifier { identifierName :: String }
          | Object { object :: [(String, Expression)]}
          | Array { elements :: [Expression]}
          | Parens { parenExpr :: Expression }
          | DoubleLiteral { double :: Double }
          | IntLiteral { integer :: Int }
          | StringLiteral { stringLiteral :: String }
          | Application { callee :: Expression, arguments :: [Expression]}
          | Index { callee :: Expression, index :: Expression }
          | Member { lhs :: Expression, member :: String}
          | Op { op :: String , lhs :: Expression, rhs :: Expression }
          | Assignment { lhs :: Expression, rhs :: Expression }
          | Function { functionName :: Maybe String, parameters :: [Id], body :: [Statement] }
          | ExprQuote { exprQuote :: String }
            deriving (Show, Data, Typeable)

class Positioned p where
    getPosition :: p -> SourcePos

instance Positioned Statement where
    getPosition = statementPosition

instance Positioned Expression where
    getPosition = expressionPosition

class FromExpr e where
    fromExpr :: Positioned p => p -> Expr -> e

instance FromExpr Expression where
    fromExpr p e = Expression (getPosition p) e

instance FromExpr Stmt where
    fromExpr p e = ExpressionStmt $ fromExpr p e

instance FromExpr Statement where
    fromExpr p e = Statement (getPosition p) $ fromExpr p e
