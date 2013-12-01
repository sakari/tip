module Language.Tip.Parser where

import Language.Tip.Ast
import Text.Parsec
import Text.Parsec.ByteString
import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec.Token

t = makeTokenParser def
    where def = LanguageDef {
                  commentStart = "/*"
                , commentEnd = "*/"
                , commentLine = "//"
                , nestedComments = True
                , identStart = letter <|> oneOf "_$"
                , identLetter = letter <|> digit <|> oneOf "_$"
                , opStart = op
                , opLetter = op
                , reservedNames = []
                , reservedOpNames = []
                , caseSensitive = True
                }
          op = oneOf "*/%-+.=<>"

parseIdentifier= Identifier <$> identifier t

parseExpression = parseExprLhs `chainl1` parseExprRhs
    where
      parseParens = Parens <$> parens t parseExpression
      parseExprTerminal = Expression
                          <$> getPosition
                          <*> (parseIdentifier <|> parseParens)
      parseApplyList = parens t $ commaSep t parseExpression
      parseIndex = brackets t $ parseExpression

      parseApplication callee = Expression <$> getPosition <*> (app <|> index)
          where
            index = Index callee <$> parseIndex
            app = Application callee <$> parseApplyList

      parseExprLhs = do
        t <- parseExprTerminal
        option t $ do
          parseApplication t

      parseAssign = reservedOp t "=" >> return Assignment
      parseOp = Op <$> operator t
      parseMember = dot t >> return Member
      parseExprRhs = do
        pos <- getPosition
        m <- parseMember <|> parseOp <|> parseAssign
        return $ \l r -> Expression pos $ m l r


parseStatement = Statement
                 <$> getPosition
                 <*> (ExpressionStmt <$> parseExpression) <* optional (semi t)
parser path = Module path <$> (whiteSpace t *> many parseStatement <* eof)


parse :: FilePath -> IO (Either ParseError Module)
parse path = parseFromFile (parser path) path
