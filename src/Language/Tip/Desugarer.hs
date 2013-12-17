{-# LANGUAGE NamedFieldPuns, QuasiQuotes, RankNTypes #-}
module Language.Tip.Desugarer (precheck, pregenerate) where
import Data.Generics
import Data.List (find)
import Language.Tip.Ast
import Language.Tip.Quote
import Language.Tip.Symbols
import qualified Data.Set as Set

precheck ast = explicitVars $ explicitReturns ast
pregenerate ast = classToFunc $ asyncTransform ast

classToFunc ast = mkT go `everywhere` ast
    where
      go Expression { expr = Class { className, properties }} =
          [tipE| (() { __init() { `initBody }; `c; `methods; return `className })() |]
              where
                c = [tip| `className (`constrParams) { __init.apply(this); `constrBody } |]
                methods = concatMap w properties
                    where
                      w Property { propertyExpr = e@Expression { expr = Function {}}
                                 , propertyName }
                          | propertyName == Id "constructor" = []
                          | otherwise = [tip| `className.prototype.`propertyName = `e |]
                      w p = []

                initBody = concatMap w properties
                    where
                      w Property { propertyExpr = Expression { expr = Function {}}} = []
                      w Property { propertyName, propertyExpr } =
                          [tip| this.`propertyName = `propertyExpr |]
                constrParams = maybe [] parameters constructor
                constrBody = maybe [] body constructor
                constructor = do
                  c <- find w properties
                  case c of
                    Property { propertyExpr =
                                   Expression
                                   { expr = f@Function {}}} -> return f
                    _ -> error $ "you should really consider giving a function literal for class constructor of: " ++ show className

                w Property { propertyName }
                    | propertyName == Id "constructor" = True
                    | otherwise = False
      go a = a

freeVars ast = symbols ast `Set.difference` declarations ast

explicitVars m@Module { moduleStatements } = m { moduleStatements = go globals moduleStatements }
    where
      globals = Set.fromList ["this", "require", "module", "exports", "true", "false", "null", "process"]

      go :: Set.Set String -> [Statement] -> [Statement]
      go free stmts = varDecls ++ stmts'
          where
            boundInBody = freeVars stmts `Set.difference` free
            free' = free `Set.union` boundInBody
            varDecls = concatMap ( \i -> [tip| var `i |] )
                       $ map Id $ Set.toList boundInBody
            stmts' = everywhereBut' (mkQ False isFun) (mkT w) stmts
            w f@Function { parameters, body } =
                f { body = go (Set.union free' $ Set.fromList $ map (idName . parameterId) parameters) body }
            w e = e
            isFun Function {} = True
            isFun _ = False

everywhereBut' :: Data a => GenericQ Bool -> GenericT -> a -> a
everywhereBut' q f x
    | q x       = f x
    | otherwise = gmapT (everywhereBut' q f) $ f x


explicitReturns ast = everywhere (mkT go) ast
    where
      go f@Function { body } = f { body = addReturnForLast body }
      go a = a

      addReturnForLast b = w $ reverse b
          where
            w (s:ss) = reverse $ (addReturn s):ss
            w [] = []

      addReturn s@Statement { stmt = e@ExpressionStmt { expression } } =
          s { stmt = ReturnStmt $ Just expression }
      addReturn s@Statement { stmt = i@IfStmt { ifBody, elseBranch }} =
          s { stmt = i { ifBody = addReturnForLast ifBody
                       , elseBranch = addReturnForLast elseBranch  }}
      addReturn s = s

asyncTransform ast = everywhere (mkT go) ast
    where
      go :: Expr -> Expr
      go f@Function { parameters, body }
          | isAsync body = f { body = asyncBody cb body }
          | otherwise = f
          where
            cb = case reverse parameters of
                   (f:_) -> f
                   [] -> error $ "missing callback for async function"
      go x = x

asyncBody cb ((s@Statement { stmt = Async { resultList, asyncCall}}):ss) =
    asyncToCall cb s resultList (expr asyncCall) ss

asyncBody cb ((s@Statement { stmt = YieldStmt y }):_) =
    [tip| return `cb(null, `y) |]

asyncBody cb (s:ss) = s : asyncBody cb ss

asyncBody cb [] = []

asyncToCall cb s resultList Application { callee, arguments } tail =
    [tip| `callee (`arguments, (err, `resultList_) {
                     `sets
                     if(err) { return `cb(err) }
                     else { `tail_ }
                   }) |]
        where
          tail_ = asyncBody cb tail
          resultList_ = map (\(p@Parameter { parameterId = Id i}) -> p { parameterId = Id $ "__" ++ i}) resultList
          sets = concat $ zipWith go resultList resultList_
          go (Parameter { parameterId = Id i}) (Parameter { parameterId = Id p})  = [tip| `i_ = `p_ |]
              where
                i_ = Expression { expr = Identifier i }
                p_ = Expression { expr = Identifier p }


asyncToCall _ _ _ _ _ = error "you should really use a function call on rhs of async arrow"

isAsync body = any go body
    where
      go Statement { stmt = Async {}} = True
      go _ = False
