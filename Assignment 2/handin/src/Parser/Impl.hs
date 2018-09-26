module Parser.Impl where

-- Put your parser implementation in this file (and, if appropriate,
-- in other files in the Parser/ subdirectory)
import SubsAst
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String

-- ord used in isLegalChar to check if char is in printable ASCII range
import Data.Char

data ParseError =
  ParseError String
  deriving (Eq, Show)

parseString :: String -> Either ParseError Expr
parseString s =
  case (parse
          (do res <- parseLeadingWhitespace parseExpr
              eof
              return res)
          "ERROR"
          s) of
    Left a -> Left (ParseError (show a))
    Right b -> Right b

posNumber :: Parser Expr
posNumber = do
  n <- many1 digit
  if length n <= 8
    then return $ Number $ read n
    else fail "Number too long"

negNumber :: Parser Expr
negNumber = do
  m <- string "-"
  n <- many1 digit
  if length n <= 8
    then return $ Number $ read (m ++ n)
    else fail "Number too long"

parseNumber :: Parser Expr
parseNumber = do
  res <- parseWhitespace (posNumber <|> negNumber)
  return res

parseParentheses :: Parser Expr
parseParentheses = do
  _ <- parseWhitespace (char '(')
  expr <- parseExpr
  _ <- parseWhitespace (char ')')
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
parseExpr = choice [parseNotComma, parseCons]

parseNotComma :: Parser Expr
parseNotComma = do
  expr1 <- parseWhitespace parseExpr'
  parseComma expr1

parseComma :: Expr -> Parser Expr
parseComma expr1 =
  (do _ <- parseWhitespace (char ',')
      expr2 <- parseWhitespace parseExpr
      return (Comma expr1 expr2)) <|>
  return expr1

keywords :: [String]
keywords = ["true", "false", "undefined", "for", "of", "if"]

parseCons :: Parser Expr
parseCons =
  choice
    [ try parseArray
    , parseArrayStart
    , try parseCall
    , parseParentheses
    , parseNumber
    , parseStr
    , parseTrue
    , parseFalse
    , parseUndefined
    , try parseAssign
    , try parseIdent
    ]

parseIdent :: Parser Expr
parseIdent = do
  fc <- letter
  rest <- many (digit <|> letter <|> char '_')
  let input = fc : rest
  if input `notElem` keywords
    then return (Var input)
    else fail "should not be a keyword"

parseAssign :: Parser Expr
parseAssign = do
  Var ident <- parseWhitespace (parseIdent)
  _ <- parseWhitespace (char '=')
  expr1 <- parseExpr'
  return (Assign ident expr1)

parseCall :: Parser Expr
parseCall = do
  Var ident <- parseWhitespace (parseIdent)
  _ <- parseWhitespace (char '(')
  exprs <- parseExprs
  _ <- parseWhitespace (char ')')
  return (Call ident exprs)

parseExprs :: Parser [Expr]
parseExprs =
  do expr1 <- parseExpr'
     parseCommaExprs expr1
     <|> return []

parseCommaExprs :: Expr -> Parser [Expr]
parseCommaExprs expr1 =
  do _ <- parseWhitespace (char ',')
     expr2 <- parseExprs
     return (expr1 : expr2)
     <|> return [expr1]

parseArrayStart :: Parser Expr
parseArrayStart = do
  _ <- parseWhitespace (char '[')
  compr <- parseArrayFor
  _ <- parseWhitespace (char ']')
  return (Compr compr)

parseArrayFor :: Parser ArrayCompr
parseArrayFor = do
  _ <- parseWhitespace (string "for")
  _ <- parseWhitespace (char '(')
  Var ident <- parseWhitespace (parseIdent)
  _ <- parseWhitespace (string "of")
  expr1 <- parseWhitespace (parseExpr')
  _ <- parseWhitespace (char ')')
  compr <- parseArrayCompr
  return (ACFor ident expr1 compr)

parseArrayCompr :: Parser ArrayCompr
parseArrayCompr = choice [try parseACBody, parseArrayFor, parseACIf]

parseACBody :: Parser ArrayCompr
parseACBody = do
  expr <- parseExpr'
  return (ACBody expr)

parseACIf :: Parser ArrayCompr
parseACIf = do
  _ <- parseWhitespace (string "if")
  _ <- parseWhitespace (char '(')
  expr1 <- parseWhitespace (parseExpr')
  _ <- parseWhitespace (char ')')
  compr <- parseArrayCompr
  return (ACIf expr1 compr)

parseArray :: Parser Expr
parseArray = do
  _ <- parseWhitespace (char '[')
  exprs <- parseExprs
  _ <- parseWhitespace (char ']')
  return (Array exprs)

-- checks that the char after the backslash is one of the legal possibilites
isLegalAfterBackslash :: Char -> Either ParseError Char
isLegalAfterBackslash c
  | c == 'n' = Right '\n'
  | c == 't' = Right '\t'
  | c `elem` ['\'', '\\'] = Right c
  | otherwise = fail "Backslash followed by invalid char"

-- extracts the char after the \ to return it together with \
isLegalBackslash :: Parser Char
isLegalBackslash = do
  _ <- char '\\'
  c <- oneOf ['\'', 'n', 't', '\\']
  case isLegalAfterBackslash c of
    Right a -> return a
    _ -> fail "Fail in Backslash"

-- checks for printable ascii chars and not \' and \\
isLegalChar :: Char -> Bool
isLegalChar c
  | c == '\'' = False
  | c == '\\' = False
  | ord c >= 32 && ord c <= 126 = True
  | otherwise = False

-- option""(try) checks for newline in string to be skipped
-- then checks for backslashes and legal chars
parseCharInStr :: Parser Char
parseCharInStr = do
  _ <- option "" (try (string "\\\n"))
  a <- isLegalBackslash <|> satisfy isLegalChar
  _ <- option "" (try (string "\\\n"))
  return a

parseStr :: Parser Expr
parseStr = do
  _ <- char '\''
  res <- many parseCharInStr
  _ <- parseWhitespace (char '\'')
  return (SubsAst.String res)

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
parseCompare =
  (do _ <- parseWhitespace (string "<")
      return (\x y -> Call "<" [x, y])) <|>
  (do _ <- parseWhitespace (string "===")
      return (\x y -> Call "===" [x, y]))

parseAdditon :: Parser Expr
parseAdditon = do
  prod <- parseWhitespace (parseProd)
  parseAdditon' prod

parseAdditon' :: Expr -> Parser Expr
parseAdditon' input =
  (do addOp <- parseWhitespace (char '+' <|> char '-')
      cons <- parseProd
      parseAdditon' $ Call [addOp] [input, cons]) <|>
  return input

parseProd :: Parser Expr
parseProd = do
  cons <- parseWhitespace (parseCons)
  parseProd' cons

parseProd' :: Expr -> Parser Expr
parseProd' input =
  (do prodOp <- parseWhitespace (char '*' <|> char '%')
      cons <- parseCons
      parseProd' $ Call [prodOp] [input, cons]) <|>
  return input
