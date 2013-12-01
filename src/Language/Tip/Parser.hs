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
                , reservedOpNames = ["="]
                , caseSensitive = True
                }
          op = oneOf "*/%-+.=<>"

parseString = lexeme t $ single <|> double
    where
      double :: Parser String
      double = qq *> manyConcat pp <*qq
      qq = string "\""
      pp = choice [string "\\\"", string "\\\\", none "\""]
      single :: Parser String
      single = q *> manyConcat p <* q
      q = string "'"
      p = choice [string "\\'", string "\\\\", none "'"]
      manyConcat parser = concat <$> many parser
      none str = noneOf str >>= \c -> return [c]

parseSymbol = identifier t <|> parseString
parseIdentifier= Identifier <$> identifier t
parseStringLiteral = StringLiteral <$> parseString

parseExpression = parseExprLhs `chainl1` parseExprRhs
    where
      parseKeyValue = do
        s <- parseSymbol
        lexeme t $ string ":"
        e <- parseExpression
        return (s, e)
      parseObject = Object <$> braces t (commaSep t $ parseKeyValue)
      parseArray = Array <$> brackets t (commaSep t parseExpression)
      parseNumber = Number . either fromIntegral id <$> naturalOrFloat t
      parseParens = Parens <$> parens t parseExpression
      parseExprTerminal = Expression
                          <$> getPosition
                          <*> (parseIdentifier
                               <|> parseParens
                               <|> parseNumber
                               <|> parseStringLiteral
                               <|> parseArray
                               <|> parseObject )
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
