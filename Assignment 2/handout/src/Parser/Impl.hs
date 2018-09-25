module Parser.Impl where

-- Put your parser implementation in this file (and, if appropriate,
-- in other files in the Parser/ subdirectory)

import           SubsAst
import           Text.Parsec
import           Text.Parsec.String
--import           Data.Char

parseString :: String -> Either ParseError Expr
parseString s = parse (do
                        res <- parseLeadingWhitespace parseExpr
                        eof
                        return res) "ERROR" s

posNumber :: Parser Expr
posNumber = do
  n <- many1 digit
  if length n <= 8 then return $ Number $ read n else fail "Number too long"

negNumber :: Parser Expr
negNumber = do
  m <- string "-"
  n <- many1 digit
  if length n <= 8 then return $ Number $ read (m ++ n) else fail "Number too long"

parseNumber :: Parser Expr
parseNumber = do
                res <- parseWhitespace(posNumber <|> negNumber)
                return res

parseParentheses :: Parser Expr
parseParentheses = do
                       _ <- parseWhitespace(char '(')
                       expr <- parseExpr
                       _ <- parseWhitespace(char ')')
                       return expr

parseComment :: Parser ()
parseComment = do
                  _ <- string "//"
                  _ <- manyTill anyChar (newLine <|> eof)
                  return ()

--makes newline be of type ()
newLine :: Parser ()
newLine = do
            _ <- newline
            return ()

parseLeadingWhitespace :: Parser a -> Parser a
parseLeadingWhitespace par = do
                                spaces
                                optional parseComment
                                spaces
                                par

parseWhitespace :: Parser a -> Parser a
parseWhitespace par = do
                        p <- par
                        spaces
                        optional parseComment
                        spaces
                        return p

-- check for comma
parseExpr :: Parser Expr
parseExpr = choice [ parseNotComma, parseCons ]

parseNotComma :: Parser Expr
parseNotComma = do
                    expr1 <- parseWhitespace parseExpr'
                    parseComma expr1

parseComma :: Expr -> Parser Expr
parseComma expr1 = (do
                _ <- parseWhitespace(char ',')
                expr2 <- parseWhitespace parseExpr'
                return (Comma expr1 expr2)) <|> return expr1


parseCons :: Parser Expr
parseCons = choice [
                parseNumber,
                parseStr,
                parseTrue,
                parseFalse,
                parseUndefined,
                try parseAssign,
                try parseCall,
                parseIdent,
                try parseArray,
                parseArrayStart,
                parseParentheses ]

parseIdent :: Parser Expr
parseIdent = do
                fc <- letter
                rest <- many (digit <|> letter <|> char '_')
                return (Var (fc:rest::String))

parseAssign :: Parser Expr
parseAssign = do
                Var ident <- parseWhitespace(parseIdent)
                _ <- parseWhitespace(char '=')
                expr1 <- parseExpr'
                return (Assign ident expr1)

parseCall :: Parser Expr
parseCall = do
                Var ident <- parseWhitespace(parseIdent)
                _ <- parseWhitespace(char '(')
                exprs <- parseExprs
                _ <- parseWhitespace(char ')')
                return (Call ident exprs)


parseExprs :: Parser [Expr]
parseExprs = do
                expr1 <- parseExpr'
                parseCommaExprs expr1
              <|> return []

parseCommaExprs :: Expr -> Parser [Expr]
parseCommaExprs expr1 = do
                            _ <- parseWhitespace(char ',')
                            expr2 <- parseExprs
                            return (expr1:expr2)
                          <|> return [expr1]

parseArrayStart :: Parser Expr
parseArrayStart = do
                    _ <- parseWhitespace(char '[')
                    compr <- parseArrayFor
                    _ <- parseWhitespace(char ']')
                    return (Compr compr)

parseArrayFor :: Parser ArrayCompr
parseArrayFor = do
                    _ <- parseWhitespace(string "for")
                    _ <- parseWhitespace(char '(')
                    Var ident <- parseWhitespace(parseIdent)
                    _ <- parseWhitespace(string "of")
                    expr1 <- parseWhitespace(parseExpr')
                    _ <- parseWhitespace(char ')')
                    compr <- parseArrayCompr
                    return (ACFor ident expr1 compr)


parseArrayCompr :: Parser ArrayCompr
parseArrayCompr = choice [ try parseACBody, parseArrayFor, parseACIf ]


parseACBody :: Parser ArrayCompr
parseACBody = do
                expr <- parseExpr'
                return (ACBody expr)

parseACIf :: Parser ArrayCompr
parseACIf = do
                _ <- string "if"
                _ <- char '('
                expr1 <- parseExpr'
                _ <- char ')'
                compr <- parseArrayCompr
                return (ACIf expr1 compr)


parseArray :: Parser Expr
parseArray = do
                _ <- parseWhitespace(char '[')
                exprs <- parseExprs
                _ <- parseWhitespace(char ']')
                return (Array exprs)

-- isLegalChar :: Char -> Bool
-- isLegalChar c | ord c >= 32 && ord c <= 126 = True
--               | otherwise = False

parseStr :: Parser Expr
parseStr = do
                _ <- parseWhitespace(char '\'')
                res <- parseWhitespace(many alphaNum)
                --res <- many (satisfy isLegalChar)
                _ <- parseWhitespace(char '\'')
                return (String res)

parseTrue :: Parser Expr
parseTrue = do
               _ <- string "true"
               return TrueConst

parseFalse :: Parser Expr
parseFalse = do
               _ <- string "false"
               return FalseConst

parseUndefined :: Parser Expr
parseUndefined = do
                    _ <- string "undefined"
                    return Undefined


parseExpr' :: Parser Expr
parseExpr' = parseAdditon `chainl1` parseCompare

parseCompare :: Parser (Expr -> Expr -> Expr)
parseCompare = (do
              _ <- parseWhitespace(string "<")
              return (\x y -> Call "<" [x, y]))
              <|> (do
                      _ <- parseWhitespace(string "===")
                      return (\x y -> Call "===" [x, y]))

parseAdditon :: Parser Expr
parseAdditon = do
                  prod <- parseProd
                  parseAdditon' prod

parseAdditon' :: Expr -> Parser Expr
parseAdditon' input = (do
                         addOp <- parseWhitespace(char '+' <|> char '-')
                         cons <- parseProd
                         parseAdditon' $ Call [addOp] [input, cons])
                         <|> return input

parseProd :: Parser Expr
parseProd = do
                cons <- parseCons
                parseProd' cons

parseProd' :: Expr -> Parser Expr
parseProd' input = (do
                       prodOp <- parseWhitespace(char '*' <|> char '%')
                       cons <- parseCons
                       parseProd' $ Call [prodOp] [input, cons])
                       <|> return input
