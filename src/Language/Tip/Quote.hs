{-# LANGUAGE NamedFieldPuns #-}
module Language.Tip.Quote (tip) where
import Language.Tip.Parser (parseStatementList)
import Language.Tip.Ast
import Text.Parsec
import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Data.ByteString.UTF8
import Control.Applicative hiding ((<|>), many, optional)

tip =  QuasiQuoter
  { quoteExp = quoteExprExp }

quoteExprExp str = do
  x <- p $ fromString str
  dataToExpQ antiQuote x

antiQuote k = (mkQ Nothing qexpr) k
    where
      qexpr ExprQuote { exprQuote } = Just $ TH.varE (TH.mkName exprQuote)
      qexpr _ = Nothing

p str = case parse (parseStatementList <* eof) "" str of
          Left e -> fail $ show e
          Right r -> return r
