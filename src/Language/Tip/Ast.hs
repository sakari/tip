{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns, TypeSynonymInstances, FlexibleInstances #-}
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

data Property = Property { propertyPosition :: SourcePos
                         , propertyName :: Id
                         , propertyExpr :: Expression }
              deriving (Show, Data, Typeable)

data Stmt = ExpressionStmt { expression :: Expression }
          | Async { resultList :: [Parameter], asyncCall :: Expression }
          | ReturnStmt { returnExpression :: Maybe Expression }
          | YieldStmt { yield :: [Expression] }
          | IfStmt { condition :: Expression
                   , ifBody :: [Statement]
                   , elseBranch:: [Statement] }
          | VarStmt { varId :: Id, varType :: Maybe Type, varAssignment :: Maybe Expression }
            deriving (Show, Data, Typeable)

data Id = Id { idName :: String }
        | IdQuote { idQuote :: String }
          deriving (Show, Data, Typeable, Eq)

data Parameter = Parameter {
      parameterId :: Id
    , parameterType :: Maybe Type
    } deriving (Show, Data, Typeable)

data Expression = Expression {
      expressionPosition :: SourcePos
    , expr :: Expr
    , exprType :: Maybe Type
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
          | Member { lhs :: Expression, member :: Id}
          | Op { op :: String , lhs :: Expression, rhs :: Expression }
          | Assignment { op :: String, lhs :: Expression, rhs :: Expression }
          | Function { functionName :: Maybe Id
                     , parameters :: [Parameter]
                     , returnType :: Maybe Type
                     , body :: [Statement] }
          | ExprQuote { exprQuote :: String }
          | New { newClass :: Expression }
          | Class { className :: Id
                  , properties :: [Property]}
          | Not { opExpr :: Expression }
          | Negate { opExpr :: Expression }
          | Plus { opExpr :: Expression }
          | PreIncrement { opExpr :: Expression }
          | PreDecrement { opExpr :: Expression }
          | PostIncrement { opExpr :: Expression }
          | PostDecrement { opExpr :: Expression }
            deriving (Show, Data, Typeable)

data Type = NumberType
          | BoolType
          | StringType
          | ConstantType { constant :: String }
          | StructType { structFields :: [(Id, Type)] }
          | ArrayType { elementType :: Type }
          | FunType { parameterTypes :: [Type], funReturnType :: Maybe Type }
          | Interface { interfaceName :: Id }
            deriving (Show, Data, Typeable)

class ToExpression a where
    toExpression :: a -> Expression

instance ToExpression String where
    toExpression = toExpression . toId

instance ToExpression Id where
    toExpression Id { idName } = Expression { expr = Identifier idName }

instance ToExpression Expression where
    toExpression = id

instance ToExpression Parameter where
    toExpression Parameter { parameterId } = toExpression parameterId

class ToStatement a where
    toStatement :: a -> Statement

instance ToStatement String where
    toStatement = toStatement . toExpression

instance ToStatement Id where
    toStatement = toStatement . toExpression

instance ToStatement Expression where
    toStatement a = Statement { stmt = ExpressionStmt { expression = a}}

instance ToStatement Statement where
    toStatement = id

class ToParameter a where
    toParameter :: a -> Parameter

instance ToParameter String where
    toParameter a = Parameter (toId a) Nothing

instance ToParameter Parameter where
    toParameter = id

instance ToParameter Id where
    toParameter a = Parameter a Nothing

class ToId a where
    toId :: a -> Id

instance ToId Id where
    toId = id

instance ToId String where
    toId = Id

class Positioned p where
    getPosition :: p -> SourcePos

instance Positioned Statement where
    getPosition = statementPosition

instance Positioned Expression where
    getPosition = expressionPosition
