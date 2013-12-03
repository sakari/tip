{-# LANGUAGE NamedFieldPuns, QuasiQuotes #-}
module Language.Tip.Desugarer (desugar) where
import Data.Generics
import Language.Tip.Ast
import Language.Tip.Quote

desugar ast = asyncTransform $ explicitReturns ast

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
          | isAsync body = f { parameters = parameters ++ [Id "cb"]
                             , body = asyncBody body }
          | otherwise = f
      go x = x

asyncBody ((s@Statement { stmt = Async { resultList, asyncCall}}):ss) =
    asyncToCall s resultList (expr asyncCall) ss
asyncBody ((s@Statement { stmt = YieldStmt y }):_) =
    [tip| return cb(null, `y) |]
asyncBody (s:ss) = s : asyncBody ss
asyncBody [] = []

asyncToCall s resultList Application { callee, arguments } tail =
    [tip| `callee (`arguments, (err, `resultList) {
                     if(err) { return cb(err) }
                     else { `tail_ }
                   }) |]
        where
          tail_ = asyncBody tail
asyncToCall _ _ _ _ = error "you should really use a function call on rhs of async arrow"

isAsync body = any go body
    where
      go Statement { stmt = Async {}} = True
      go _ = False
