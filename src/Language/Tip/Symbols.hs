{-# LANGUAGE NamedFieldPuns #-}
module Language.Tip.Symbols where
import Language.Tip.Ast
import qualified Data.Set as Set
import Data.Generics

symbols :: Data a => a -> Set.Set String
symbols = everythingBut Set.union $ mkQ (Set.empty, False) ident `extQ` async
    where
      ident Function {} = (Set.empty, True)
      ident Identifier { identifierName } = (Set.singleton identifierName, False)
      ident _ = (Set.empty, False)

      async Async { resultList } = (Set.fromList $ map (idName .parameterId) resultList, False)
      async _ = (Set.empty, False)

declarations :: Data a => a -> Set.Set String
declarations = everythingBut Set.union $ mkQ (Set.empty, False) vars `extQ` notFun
    where
      notFun Function { functionName } = case functionName of
                                           Nothing -> (Set.empty, True)
                                           Just n -> (Set.singleton $ idName n, True)
      notFun _ = (Set.empty, False)
      vars VarStmt { varId } = ( Set.singleton $ idToString varId, False)
      vars _ = (Set.empty, False)

idToString Id { idName } = idName
idToString IdQuote { idQuote } = error $ "cannot get id name from quote: " ++ idQuote
