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

generateExpr Expression { expr } = x expr
    where
      x :: Expr -> J.Expression ()
      x Function { functionName, parameters, body } =
          J.FuncExpr () (J.Id () `fmap` functionName) (map (J.Id () . idName) parameters) $
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
      x Class { className, properties } = generateClass className properties
      x New { newClass = Expression { expr = Application { callee, arguments }}} =
          J.NewExpr () (generateExpr callee ) $ map generateExpr arguments
      x e = error $ "missing expression case: " ++ show e

generateClass i properties = container
    where
      container = J.CallExpr () (J.FuncExpr () Nothing [] containerBody ) []
      containerBody = [initFunc, J.ExprStmt () classFunc] ++ prototypeMethods ++
                      [J.ReturnStmt () $ Just $ J.VarRef () $ J.Id () className]
      classFunc = J.FuncExpr() (Just $ J.Id () className) constrArgs constrBody
      prototypeMethods = map (J.ExprStmt ()) $ concatMap go properties
          where
            go Property { propertyExpr = e@Expression { expr = Function {}}, propertyName}
                | propertyName == Id "constructor" = []
                | otherwise = [J.AssignExpr () J.OpAssign
                                    (J.LDot () (J.DotRef ()
                                                     (J.VarRef () $ J.Id () className)
                                                     (J.Id () "prototype"))
                                    $ idName propertyName)
                               (generateExpr e)]
            go _ = []
      constr = do
        c <- find ((==) (Id "constructor") . propertyName) properties
        case propertyExpr c of
          Expression { expr = f@Function {} } -> return f
          _ -> error "contructor must be a function"
      className = idName i
      constrArgs = map (J.Id () . idName) $ maybe [] parameters constr
      constrBody = initCall:(map generateStmt $ maybe [] body constr)
      initCall = J.ExprStmt () $ J.CallExpr ()
                 (J.DotRef() (J.VarRef () $ J.Id () "__init__") $ J.Id () "apply") [
                  J.VarRef () $ J.Id () "this"
                 ]
      initFunc = J.ExprStmt () $ J.FuncExpr () (Just $ J.Id () "__init__") [] $ initBody
      initBody = map (J.ExprStmt ()) $ concatMap go properties
          where
            go Property { propertyExpr = Expression { expr = Function {}}} = []
            go Property { propertyName, propertyExpr } = [
             J.AssignExpr () J.OpAssign
                  (J.LDot () (J.VarRef () $ J.Id () "this")
                              (idName propertyName ))
                  (generateExpr propertyExpr)]

generateLvalue Expression { expr } = x expr
    where
      x Identifier { identifierName } = J.LVar () identifierName
      x Member { lhs, member } = J.LDot () (generateExpr lhs) member
      x Index { callee, index } = J.LBracket () (generateExpr callee) (generateExpr index)
      x n = error $ "invalid lvalue:" ++ show n
