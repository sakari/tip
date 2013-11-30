module Language.Tip.Parser where

import Language.Tip.Ast
import Text.Parsec
import Text.Parsec.ByteString
import Text.Parsec.Token
import Control.Applicative hiding ((<|>), many)

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


parseIdentifier = Identifier <$> identifier t

parseExprTerminal = Expression <$> getPosition <*> parseIdentifier

parseApplyList = parens t $ commaSep t parseExpression

parseApplication callee = Expression <$> getPosition <*> app
    where
      app = Application callee <$> parseApplyList


parseExpression = do
  t <- parseExprTerminal
  option t $ do
    parseApplication t

parseStatement = Statement <$> getPosition <*> (ExpressionStmt <$> parseExpression)
parser path = Module path <$> (whiteSpace t *> many parseStatement <* eof)


parse :: FilePath -> IO (Either ParseError Module)
parse path = parseFromFile (parser path) path
