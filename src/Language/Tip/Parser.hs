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
parseExpression = Expression <$> getPosition <*> parseIdentifier
parseStatement = Statement <$> getPosition <*> (ExpressionStmt <$> parseExpression)
parser path = Module path <$> many parseStatement

parse :: FilePath -> IO (Either ParseError Module)
parse path = parseFromFile (parser path) path
