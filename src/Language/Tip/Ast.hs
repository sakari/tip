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

data Stmt = ExpressionStmt { expression :: Expression }
          | ReturnStmt { returnExpression :: Maybe Expression }
            deriving Show


data Expression = Expression {
      expressionPosition :: SourcePos
    , expr :: Expr
    } deriving Show

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
            deriving Show
