{-# LANGUAGE NamedFieldPuns #-}
module Language.Tip.Generator (generate) where
import Language.Tip.Ast
import qualified Language.ECMAScript3.Syntax as J

generate :: Module -> [J.Statement ()]
generate Module { moduleStatements } = map generateStmt moduleStatements

generateBody :: [Statement] -> [J.Statement ()]
generateBody stmts = go stmts
    where
      go [s] = [addReturn s]
      go (s:ss) = generateStmt s: go ss
      go [] = []
      addReturn s@Statement { stmt = ReturnStmt {} } =
          generateStmt s
      addReturn s@Statement { stmt = ExpressionStmt { expression }} =
          J.ReturnStmt () $ Just $ generateExpr expression
      addReturn s = generateStmt' generateBody s


generateStmt stmt = generateStmt' (map generateStmt) stmt

generateStmt' bodyGenerator Statement { stmt } = x stmt
    where
      x ExpressionStmt { expression } =
          J.ExprStmt () $ generateExpr expression
      x ReturnStmt { returnExpression } =
          J.ReturnStmt () $ generateExpr `fmap` returnExpression
      x IfStmt { condition, ifBody, elseBranch } =
          J.IfStmt () (generateExpr condition)
               (J.BlockStmt () $ bodyGenerator ifBody)
               (J.BlockStmt () $ bodyGenerator elseBranch)


generateExpr Expression { expr } = x expr
    where
      x :: Expr -> J.Expression ()
      x Function { functionName, parameters, body } =
          J.FuncExpr () (J.Id () `fmap` functionName) (map (J.Id () . idName) parameters) $
           generateBody body
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
      x Member { lhs, member} = J.DotRef () (generateExpr lhs) (J.Id () member)
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
      x Assignment { lhs, rhs } = J.AssignExpr () J.OpAssign (generateLvalue lhs) (generateExpr rhs)
      x ExprQuote { exprQuote } = J.StringLit () exprQuote

generateLvalue Expression { expr } = x expr
    where
      x Identifier { identifierName } = J.LVar () identifierName
      x Member { lhs, member } = J.LDot () (generateExpr lhs) member
      x Index { callee, index } = J.LBracket () (generateExpr callee) (generateExpr index)
      x n = error $ "invalid lvalue:" ++ show n
