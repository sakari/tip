{-# LANGUAGE NamedFieldPuns, TemplateHaskell, RankNTypes #-}
module Language.Tip.Quote (tip, tipE) where
import Language.Tip.Parser (parseStatementList, parseExpression, parser)
import Language.Tip.Ast
import Text.Parsec
import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Data.ByteString.UTF8 hiding (foldl)
import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad

tipModule = QuasiQuoter { quoteExp = quoteModule }
    where
      quoteModule str = do
        x <- parseTipModule $ fromString str
        dataToExpQ antiQuote x

tipE = QuasiQuoter
       { quoteExp = quoteExpression }

quoteExpression str = do
  x <- parseTipExpression $ fromString str
  dataToExpQ antiQuote x

tip =  QuasiQuoter
  { quoteExp = quoteStatement }

quoteStatement str = do
  x <- parseTipStatement $ fromString str
  dataToExpQ antiQuote x

mkList p = TH.UInfixE p (TH.ConE $ TH.mkName ":") (TH.ConE $ TH.mkName "[]")
cat l r = TH.UInfixE l (TH.VarE $ TH.mkName "++") r

antiQuote :: (forall b. Data b => b -> Maybe (TH.Q TH.Exp))
antiQuote k = mkQ Nothing qexpr `extQ` qexprList `extQ` qstmtList `extQ` qparamList `extQ` qid `extQ` qstmt $  k
    where
      qid IdQuote { idQuote } = Just $ return $ liftId `TH.AppE` var idQuote
      qid i = Nothing

      qparamList :: [Parameter] -> Maybe (TH.Q TH.Exp)
      qparamList = splice liftParameter w
          where
            w Parameter { parameterId = IdQuote { idQuote }} = Just idQuote
            w _ = Nothing

      qstmt Statement { stmt = ExpressionStmt { expression = Expression { expr = ExprQuote { exprQuote }}}} =
          Just $ return $ liftStatement `TH.AppE` var exprQuote
      qstmt i = Nothing

      qstmtList :: [Statement] -> Maybe (TH.Q TH.Exp)
      qstmtList = splice liftStatement w
          where
            w Statement { stmt = ExpressionStmt { expression = Expression { expr = ExprQuote { exprQuote }}}} = Just exprQuote
            w _ = Nothing

      qexpr :: Expression -> Maybe (TH.Q TH.Exp)
      qexpr Expression { expr = ExprQuote { exprQuote }} =
          Just $ return $ liftExpression `TH.AppE` var exprQuote
      qexpr _ = Nothing

      qexprList :: [Expression] -> Maybe (TH.Q TH.Exp)
      qexprList = splice liftExpression w
          where
            w Expression { expr = ExprQuote { exprQuote }} = Just exprQuote
            w _ = Nothing

var :: String -> TH.Exp
var = TH.VarE . TH.mkName

liftParameter = TH.VarE $ TH.mkName "toParameter"

liftId = TH.VarE $ TH.mkName "toId"

liftExpression = TH.VarE $ TH.mkName "toExpression"

liftStatement = TH.VarE $ TH.mkName "toStatement"

thmap :: TH.Exp -> TH.Exp -> TH.Exp
thmap a = TH.AppE $ (TH.VarE $ TH.mkName "map") `TH.AppE` a

splice lift quote ls = Just $ do
           i <- [e| [] |]
           foldM go i ls
                 where
                   go p x = case quote x of
                              Nothing -> do
                                x' <- dataToExpQ antiQuote x
                                return $ cat p $ mkList x'
                              Just q -> return $ cat p $ (TH.VarE $ TH.mkName "map") `TH.AppE` lift `TH.AppE` TH.VarE (TH.mkName q)

parseTipStatement str = case parse (parseStatementList <* eof) "" str of
                          Left e -> fail $ show e
                          Right r -> return r

parseTipExpression str = case parse (parseExpression <* eof) "" str of
                           Left e -> fail $ show e
                           Right r -> return r

parseTipModule str = case parse (parser "nofile" <* eof) "" str of
                       Left e -> fail $ show e
                       Right r -> return r
