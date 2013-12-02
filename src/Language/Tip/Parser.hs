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
                , reservedNames = ["return", "if", "else"]
                , reservedOpNames = ["=", "<-"]
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
      parseFunction = try $ Function
             <$> optionMaybe (identifier t)
             <*> parens t (commaSep t $ identifier t)
             <*> braces t (many parseStatement)

      parseKeyValue = do
        s <- parseSymbol
        lexeme t $ string ":"
        e <- parseExpression
        return (s, e)
      parseObject = Object <$> braces t (commaSep t $ parseKeyValue)
      parseArray = Array <$> brackets t (commaSep t parseExpression)
      parseNumber = either (IntLiteral . fromIntegral) DoubleLiteral
                    <$> naturalOrFloat t
      parseParens = Parens <$> parens t parseExpression
      parseExprTerminal = Expression
                          <$> getPosition
                          <*> (parseFunction
                               <|> parseIdentifier
                               <|> parseParens
                               <|> parseNumber
                               <|> parseStringLiteral
                               <|> parseArray
                               <|> parseObject )
      parseApplyList = parens t $ commaSep t parseExpression
      parseIndex = brackets t $ parseExpression
      parseApplication callee = Expression
                                <$> getPosition
                                <*> (app <|> index <|> member)
          where
            member = Member callee <$> (dot t *> identifier t)
            index = Index callee <$> parseIndex
            app = Application callee <$> parseApplyList

      parseAppChain t = do
        option t $ do
          k <- parseApplication t
          parseAppChain k

      parseExprLhs = parseExprTerminal >>= parseAppChain

      parseAssign = reservedOp t "=" >> return Assignment
      parseOp = Op <$> operator t
      parseExprRhs = do
        pos <- getPosition
        m <- parseOp <|> parseAssign
        return $ \l r -> Expression pos $ m l r

statementOrBody = braces t (many parseStatement) <|> return `fmap` parseStatement

parseStatement = Statement
                 <$> getPosition
                 <*> stmt <* optional (semi t)
    where
      stmt = ret <|> conditional <|> async <|> expr
      async = try $ Async
              <$> commaSep t (identifier t)
              <*> (reservedOp t "<-" *> parseExpression)
      conditional = IfStmt
                    <$> (reserved t "if" *> parens t parseExpression)
                    <*> statementOrBody
                    <*> option [] (reserved t "else" *> statementOrBody)
      ret = ReturnStmt <$> (reserved t "return" *> optionMaybe parseExpression)
      expr = ExpressionStmt <$> parseExpression
parser path = Module path <$> (whiteSpace t *> many parseStatement <* eof)


parse :: FilePath -> IO (Either ParseError Module)
parse path = parseFromFile (parser path) path
