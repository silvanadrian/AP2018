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

parseNumber :: Parser Expr
parseNumber = do
                res <- posNumber
                return res

parseExpr :: Parser Expr
parseExpr = choice [ parseCons ]

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


