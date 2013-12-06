module Language.Tip.Parser where

import Language.Tip.Ast hiding (getPosition)
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
                , reservedNames = ["var",    "return", "if", "else"
                                  , "yield", "class",  "new"]
                , reservedOpNames = ["=", "<-", "`", "!"]
                , caseSensitive = True
                }
          op = oneOf "*/%-+.=<>`"

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

parseId = (IdQuote <$> (reservedOp t "`" *> identifier t))
          <|> (Id <$> identifier t)

parseExpression = parseExprLhs `chainl1` parseExprRhs
    where
      parseFunction = try $ Function
             <$> optionMaybe (identifier t)
             <*> parens t (commaSep t parseId)
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
      parseQuote = ExprQuote <$> (reservedOp t "`" *> identifier t)
      parseNew = New <$> (reserved t "new" *> parseExpression)
      parseClass = Class
                   <$> (reserved t "class" *> parseId )
                   <*> braces t (many $ parseProperty)
      parseNot = Not <$> (reservedOp t "!" *> parseExpression )
      parseExprTerminal = Expression
                          <$> getPosition
                          <*> (parseNot
                               <|> parseFunction
                               <|> parseClass
                               <|> parseNew
                               <|> parseIdentifier
                               <|> parseParens
                               <|> parseNumber
                               <|> parseStringLiteral
                               <|> parseArray
                               <|> parseObject
                               <|> parseQuote )
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

statementOrBody = braces t parseStatementList <|> return `fmap` parseStatement

parseStatementList = whiteSpace t *> many parseStatement

parseProperty = Property
                <$> getPosition
                <*> parseId
                <*> (reservedOp t "=" *> parseExpression <* optional (semi t))

parseStatement = Statement
                 <$> getPosition
                 <*> stmt <* optional (semi t)
    where
      stmt = ret <|> yield <|> var <|> conditional <|> async <|> expr
      var = VarStmt
            <$> (reserved t "var" *> parseId)
            <*> optionMaybe (reservedOp t "=" *> parseExpression)
      async = try $ Async
              <$> commaSep t parseId
              <*> (reservedOp t "<-" *> parseExpression)
      conditional = IfStmt
                    <$> (reserved t "if" *> parens t parseExpression)
                    <*> statementOrBody
                    <*> option [] (reserved t "else" *> statementOrBody)
      yield = YieldStmt <$> (reserved t "yield" *> commaSep t parseExpression)
      ret = ReturnStmt <$> (reserved t "return" *> optionMaybe parseExpression)
      expr = ExpressionStmt <$> parseExpression
parser path = Module path <$> (whiteSpace t *> many parseStatement <* eof)


parse :: FilePath -> IO (Either ParseError Module)
parse path = parseFromFile (parser path) path
