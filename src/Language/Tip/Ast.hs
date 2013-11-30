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

data Expr = Identifier { identifierName :: String } deriving Show
