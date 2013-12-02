{-# LANGUAGE DeriveDataTypeable #-}
module Language.Tip.Ast where

import Data.Typeable
import Data.Data
import Text.Parsec

data Module = Module {
      moduleFilePath :: FilePath
    , moduleStatements :: [Statement]
    } deriving (Show, Data, Typeable)

data Statement = Statement {
      statementPosition :: SourcePos
    , stmt :: Stmt
    } deriving (Show, Data, Typeable)

data Stmt = ExpressionStmt { expression :: Expression }
          | Async { resultList :: [String], asyncCall :: Expression }
          | ReturnStmt { returnExpression :: Maybe Expression }
          | IfStmt { condition :: Expression
                   , ifBody :: [Statement]
                   , elseBranch:: [Statement] }
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
          | Function { functionName :: Maybe String, parameters :: [String], body :: [Statement] }
            deriving (Show, Data, Typeable)
