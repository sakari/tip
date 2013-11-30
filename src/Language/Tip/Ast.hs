module Language.Tip.Ast where

import Text.Parsec

data Module = Module {
      moduleFilePath :: FilePath
    , moduleStatements :: [Statement]
    }

data Statement = Statement {
      statementPosition :: SourcePos
    , stmt :: Stmt
    }

data Stmt = ExpressionStmt {
      exper :: Expression
    }

data Expression = Expression {
      expressionPosition :: SourcePos
    , expr :: Expr
    }

data Expr = Identifier { identifierName :: String }
