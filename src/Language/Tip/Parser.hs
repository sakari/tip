module Language.Tip.Parser where

import Language.Tip.Ast hiding (getPosition)
import Text.Parsec
import Text.Parsec.Expr
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
                , reservedOpNames = ":" : "->" : (concatMap (map fst) exprTable)
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

parseParameter = Parameter
                 <$> parseId
                 <*> parseTypeAssignment

parseExpression = whiteSpace t *> go
    where
      go = buildExpressionParser exprTable' $ parseExprTerminal >>= parseAppChain
      exprTable' = map (map snd) exprTable
      parseFunction = try $ Function
             <$> optionMaybe parseId
             <*> parens t (commaSep t parseParameter)
             <*> parseTypeAssignment
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
                          <*> parseTypeAssignment
      parseApplyList = parens t $ commaSep t parseExpression
      parseIndex = brackets t $ parseExpression
      parseApplication callee = Expression
                                <$> getPosition
                                <*> (app <|> index <|> member)
                                <*> parseTypeAssignment
          where
            member = Member callee <$> (dot t *> parseId)
            index = Index callee <$> parseIndex
            app = Application callee <$> parseApplyList

      parseAppChain t = do
        option t $ do
          k <- parseApplication t
          parseAppChain k

parseTypeAssignment = optionMaybe ( reservedOp t ":" *> parseType)

parseType = stringtype <|> boolean <|> number <|> constant <|> struct <|> array <|> fun <|> iface
    where
      stringtype = lexeme t (string "string") >> return StringType
      iface = Interface <$> parseId
      fun = FunType
            <$> (parens t $ commaSep t parseType)
            <*> (optionMaybe $ (reservedOp t "->" *> parseType))
      array = ArrayType <$> (brackets t $ parseType)
      structField = do
        k <- parseId
        reservedOp t ":"
        v <- parseType
        return $ (k, v)
      struct = StructType <$> (braces t $ commaSep t $ structField)
      constant = ConstantType <$> parseString
      boolean = lexeme t (string "boolean") >> return BoolType
      number = lexeme t (string "number") >> return NumberType

exprTable = [ [ prefix "++" PreIncrement
              , prefix "--" PreDecrement
              , postfix "++" PostIncrement
              , postfix "--" PostDecrement]
            , [prefix "!" Not
              , prefix "-" Negate
              , prefix "+" Plus ]
            , binops ["/", "*", "%"]
            , binops ["+", "-"]
            , binops ["<<", ">>", ">>>"]
            , binops ["<", "<=", ">=", ">"]
            , binops ["==", "!="]
            , binops ["&"]
            , binops ["^"]
            , binops ["|"]
            , binops ["&&"]
            , binops ["||"]
            , assignments ["="
                          , "+=", "-=", "*=", "/=", "%="
                          , "<<=", ">>=", "<<<="
                          , "&=", "^=", "|="]
            ]

assignments names = map (binary Assignment AssocRight) names
binops names = map (binary Op AssocLeft) names

binary c assoc name =
    (name, flip Infix assoc $ do
       p <- getPosition
       reservedOp t name
       ty <- parseTypeAssignment
       return $ \l r -> Expression p (c name l r) ty)

unary name fun c =
    (name, c $ do
       p <- getPosition
       reservedOp t name
       ty <- parseTypeAssignment
       return $ \e -> Expression p (fun e) ty)

prefix name fun = unary name fun Prefix
postfix name fun = unary name fun Postfix

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
            <*> parseTypeAssignment
            <*> optionMaybe (reservedOp t "=" *> parseExpression)
      async = try $ Async
              <$> commaSep t parseParameter
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
