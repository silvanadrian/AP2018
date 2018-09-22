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
  if length n <= 8 then return $ Number $ read n else fail "Int too long"


negNumber :: Parser Expr
negNumber = do
  _ <- char '-'
  n <- many1 digit
  if length n <= 8 then return $ Number $ read n * (-1) else fail "Int too long"

parseNumber :: Parser Expr
parseNumber = do
                res <- posNumber <|> negNumber
                return res

parseExpr :: Parser Expr
parseExpr = choice [ parseCompare, parseCons ]

parseCons :: Parser Expr
parseCons = choice [ parseNumber, parseStr, parseTrue, parseFalse, parseUndefined, parseIdent ]

parseIdent :: Parser Expr
parseIdent = do
                fc <- letter
                rest <- many (digit <|> letter <|> char '_')
                return (Var (fc:rest::String))

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


parseCompare :: Parser Expr
parseCompare = do
                   addProd <- parseAdditon
                   parseCompare' addProd


parseCompare' :: Expr -> Parser Expr
parseCompare' input = (do
                       compOp <- string "===" <|> string "<"
                       cons <- parseCons
                       parseCompare' $ Call compOp [input, cons])
                       <|> return input

parseAdditon :: Parser Expr
parseAdditon = do
                  prod <- parseProd
                  parseAdditon' prod


parseAdditon' :: Expr -> Parser Expr
parseAdditon' input = (do
                         addOp <- char '+' <|> char '-'
                         cons <- parseCons
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
