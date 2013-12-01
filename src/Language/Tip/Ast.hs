module Language.Tip.Ast where

import Text.Parsec

data Module = Module {
      moduleFilePath :: FilePath
    , moduleStatements :: [Statement]
    } deriving Show

data Statement = Statement {
      statementPosition :: SourcePos
    , stmt :: Stmt
    } deriving Show

data Stmt = ExpressionStmt {
      expression :: Expression
    } deriving Show

data Expression = Expression {
      expressionPosition :: SourcePos
    , expr :: Expr
    } deriving Show

data Expr = Identifier { identifierName :: String }
          | Parens { parenExpr :: Expression }
          | Application { callee :: Expression, arguments :: [Expression]}
          | Index { callee :: Expression, index :: Expression }
          | Member { lhs :: Expression, rhs :: Expression}
          | Op { op :: String , lhs :: Expression, rhs :: Expression }
          | Assignment { lhs :: Expression, rhs :: Expression }
            deriving Show
