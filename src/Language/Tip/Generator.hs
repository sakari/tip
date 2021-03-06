{-# LANGUAGE NamedFieldPuns, QuasiQuotes #-}
module Language.Tip.Generator (generate) where
import Language.Tip.Ast
import qualified Language.ECMAScript3.Syntax as J
import Language.ECMAScript3.Syntax.QuasiQuote
import Data.List

generate :: Module -> [J.Statement ()]
generate Module { moduleStatements } = map generateStmt moduleStatements

generateStmt Statement { stmt } = x stmt
    where
      x ExpressionStmt { expression } =
          J.ExprStmt () $ generateExpr expression
      x ReturnStmt { returnExpression } =
          J.ReturnStmt () $ generateExpr `fmap` returnExpression
      x IfStmt { condition, ifBody, elseBranch } =
          J.IfStmt () (generateExpr condition)
               (J.BlockStmt () $ map generateStmt ifBody)
               (J.BlockStmt () $ map generateStmt elseBranch)
      x VarStmt { varId, varAssignment } =
          J.VarDeclStmt () [J.VarDecl () (J.Id () $ idName varId) $ fmap generateExpr varAssignment]

generateExpr Expression { expr } = x expr
    where
      x :: Expr -> J.Expression ()
      x Function { functionName, parameters, body } =
          J.FuncExpr () ((J.Id () . idName) `fmap` functionName) (map (J.Id () . idName . parameterId) parameters) $
           map generateStmt body
      x Identifier { identifierName } =
          J.VarRef () $ J.Id () identifierName
      x Object { object } = J.ObjectLit () $ map g object
          where g (k, v) = (J.PropString () k, generateExpr v)
      x Array { elements } = J.ArrayLit () $ map generateExpr elements
      x Parens { parenExpr } = generateExpr parenExpr
      x DoubleLiteral { double } = J.NumLit () double
      x IntLiteral { integer } = J.IntLit () integer
      x StringLiteral { stringLiteral } = J.StringLit () stringLiteral
      x Application { callee, arguments} = J.CallExpr () (generateExpr callee) $
                                           map generateExpr arguments
      x Index { callee, index} = J.BracketRef () (generateExpr callee) (generateExpr index)
      x Member { lhs, member} = J.DotRef () (generateExpr lhs) (J.Id () $ idName member)
      x Not { opExpr } = prefix J.PrefixLNot opExpr
      x Negate { opExpr } = prefix J.PrefixMinus opExpr
      x Plus { opExpr } = prefix J.PrefixPlus opExpr
      x Op { op, lhs, rhs } = J.InfixExpr () iop (generateExpr lhs) (generateExpr rhs)
          where
            iop | op == "<" = J.OpLT
                | op == ">" = J.OpGT
                | op == "==" = J.OpStrictEq
                | op == ">=" = J.OpGEq
                | op == "<=" = J.OpLEq
                | op == "+" = J.OpAdd
                | op == "%" = J.OpMod
                | op == "/" = J.OpDiv
                | op == "-" = J.OpSub
                | op == "*" = J.OpMul
                | otherwise = error $ "tbd: " ++ show op
      x Assignment { op, lhs, rhs } = assign op lhs rhs
      x ExprQuote { exprQuote } = J.StringLit () exprQuote
      x New { newClass = Expression { expr = Application { callee, arguments }}} =
          J.NewExpr () (generateExpr callee ) $ map generateExpr arguments
      x PostIncrement { opExpr } = unaryAssign J.PostfixInc opExpr
      x PostDecrement { opExpr } = unaryAssign J.PostfixDec opExpr
      x PreIncrement { opExpr } = unaryAssign J.PrefixInc opExpr
      x PreDecrement { opExpr } = unaryAssign J.PrefixDec opExpr
      x e = error $ "missing expression case: " ++ show e

unaryAssign o e = J.UnaryAssignExpr () o (generateLvalue e)

prefix c e = J.PrefixExpr () c $ generateExpr e
assign o lhs rhs = J.AssignExpr () op (generateLvalue lhs) (generateExpr rhs)
    where
      op = case o of
             "=" -> J.OpAssign
             "+=" -> J.OpAssignAdd
             "-=" -> J.OpAssignSub
             "*=" -> J.OpAssignMul
             "/=" -> J.OpAssignDiv
             "%=" -> J.OpAssignMod
             "^=" -> J.OpAssignBXor
             "&=" -> J.OpAssignBAnd
             "|=" -> J.OpAssignBOr
             _ -> error $ "unknown assign op: " ++ o

generateLvalue Expression { expr } = x expr
    where
      x Identifier { identifierName } = J.LVar () identifierName
      x Member { lhs, member } = J.LDot () (generateExpr lhs) $ idName member
      x Index { callee, index } = J.LBracket () (generateExpr callee) (generateExpr index)
      x n = error $ "invalid lvalue:" ++ show n
