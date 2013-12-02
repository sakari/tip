{-# LANGUAGE NamedFieldPuns, QuasiQuotes #-}
module Language.Tip.Desugarer (desugar) where
import Data.Generics
import Language.Tip.Ast
import Language.Tip.Quote

desugar ast = asyncTransform ast

ifStmt s c t f = Statement { statementPosition = pos
                           , stmt = IfStmt { condition = c
                                           , ifBody = t
                                           , elseBranch = f
                                           }
                           }
    where
      pos = statementPosition s

returnStmt s e = Statement { statementPosition = pos
                           , stmt = ReturnStmt $ go `fmap` e
                           }
    where
      go = Expression pos
      pos = getPosition s

asyncTransform ast = mkT go ast
    where
      go :: Expr -> Expr
      go f@Function { parameters, body }
          | isAsync body = f { parameters = parameters ++ ["@cb"]
                             , body = asyncBody body }
          | otherwise = f
      go x = x

asyncBody (s@Statement { stmt = Async { resultList, asyncCall}}:ss) =
    asyncToCall s resultList asyncCall ss
asyncBody (s:ss) = s : asyncBody ss

asyncToCall s resultList asyncCall tail = error "tbd"

isAsync body = any go body
    where
      go Statement { stmt = Async {}} = True
      go _ = False
