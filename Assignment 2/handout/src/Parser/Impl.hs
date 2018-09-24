module Parser.Impl where

-- Put your parser implementation in this file (and, if appropriate,
-- in other files in the Parser/ subdirectory)

import           SubsAst
import           Text.Parsec
import           Text.Parsec.String

parseString :: String -> Either ParseError Expr
parseString s = parse (do
                        res <- parseExpr
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
                res <- posNumber <|> negNumber
                return res


-- check for comma
parseExpr :: Parser Expr
parseExpr = choice [ parseNotComma, parseCons ]

parseNotComma :: Parser Expr
parseNotComma = do
                    expr1 <- parseExpr'
                    parseComma expr1

parseComma :: Expr -> Parser Expr
parseComma expr1 = (do
                _ <- char ','
                expr2 <- parseExpr'
                return (Comma expr1 expr2)) <|> return expr1


parseCons :: Parser Expr
parseCons = choice [ parseNumber, parseStr, parseTrue, parseFalse, parseUndefined, try parseAssign, parseIdent ]

parseIdent :: Parser Expr
parseIdent = do
                fc <- letter
                rest <- many (digit <|> letter <|> char '_')
                return (Var (fc:rest::String))

parseAssign :: Parser Expr
parseAssign = do
                Var ident <- parseIdent
                _ <- char '='
                expr1 <- parseExpr'
                return (Assign ident expr1)



parseStr :: Parser Expr
parseStr = do
                _ <- char '\''
                res <- many letter
                _ <- char '\''
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
              _ <- string "<"
              return (\x y -> Call "<" [x, y]))
              <|> (do
                      _ <- string "==="
                      return (\x y -> Call "===" [x, y]))

parseAdditon :: Parser Expr
parseAdditon = do
                  prod <- parseProd
                  parseAdditon' prod

parseAdditon' :: Expr -> Parser Expr
parseAdditon' input = (do
                         addOp <- char '+' <|> char '-'
                         cons <- parseProd
                         parseAdditon' $ Call [addOp] [input, cons])
                         <|> return input

parseProd :: Parser Expr
parseProd = do
                cons <- parseCons
                parseProd' cons

parseProd' :: Expr -> Parser Expr
parseProd' input = (do
                       prodOp <- char '*' <|> char '%'
                       cons <- parseCons
                       parseProd' $ Call [prodOp] [input, cons])
                       <|> return input
