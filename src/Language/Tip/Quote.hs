{-# LANGUAGE NamedFieldPuns, TemplateHaskell, RankNTypes #-}
module Language.Tip.Quote (tip) where
import Language.Tip.Parser (parseStatementList)
import Language.Tip.Ast
import Text.Parsec
import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Data.ByteString.UTF8 hiding (foldl)
import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad

tip =  QuasiQuoter
  { quoteExp = quoteExprExp }

quoteExprExp str = do
  x <- parseTip $ fromString str
  dataToExpQ antiQuote x

mkList p = TH.UInfixE p (TH.ConE $ TH.mkName ":") (TH.ConE $ TH.mkName "[]")
cat l r = TH.UInfixE l (TH.VarE $ TH.mkName "++") r

antiQuote :: (forall b. Data b => b -> Maybe (TH.Q TH.Exp))
antiQuote k = mkQ Nothing qexpr `extQ` qexprList `extQ` qstmtList `extQ` qidList `extQ` qid `extQ` qstmt $  k
    where
      qid IdQuote { idQuote } = Just $ return $ TH.VarE $ TH.mkName idQuote
      qid i = Nothing

      qidList :: [Id] -> Maybe (TH.Q TH.Exp)
      qidList = splice w
          where
            w IdQuote { idQuote } = Just idQuote
            w _ = Nothing

      qstmt Statement { stmt = ExpressionStmt { expression = Expression { expr = ExprQuote { exprQuote }}}} = Just $ return $ TH.VarE $ TH.mkName exprQuote
      qstmt i = Nothing

      qstmtList :: [Statement] -> Maybe (TH.Q TH.Exp)
      qstmtList = splice w
          where
            w Statement { stmt = ExpressionStmt { expression = Expression { expr = ExprQuote { exprQuote }}}} = Just exprQuote
            w _ = Nothing

      qexpr :: Expression -> Maybe (TH.Q TH.Exp)
      qexpr Expression { expr = ExprQuote { exprQuote }} = Just $ TH.varE (TH.mkName exprQuote)
      qexpr _ = Nothing

      qexprList :: [Expression] -> Maybe (TH.Q TH.Exp)
      qexprList = splice w
          where
            w Expression { expr = ExprQuote { exprQuote }} = Just exprQuote
            w _ = Nothing

splice quote ls = Just $ do
           i <- [e| [] |]
           foldM go i ls
                 where
                   go p x = case quote x of
                              Nothing -> do
                                x' <- dataToExpQ antiQuote x
                                return $ cat p $ mkList x'
                              Just q -> return $ cat p $ TH.VarE (TH.mkName q)

parseTip str = case parse (parseStatementList <* eof) "" str of
          Left e -> fail $ show e
          Right r -> return r
