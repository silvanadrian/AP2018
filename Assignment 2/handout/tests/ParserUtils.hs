module ParserUtils where

import SubsAst
import SubsParser
import           Text.Parsec
import           Text.Parsec.String

-- for testing parseNumber
numberParser :: String -> Either ParseError Expr
numberParser s = parse (do
                    res <- parseNumber
                    eof
                    return res) "ERROR" s

-- for testing parseStr
stringParser :: String -> Either ParseError Expr
stringParser s = parse (do
                    res <- parseStr
                    eof
                    return res) "ERROR" s

-- for testing parseFalse
falseParser :: String -> Either ParseError Expr
falseParser s = parse (do
                    res <- parseFalse
                    eof
                    return res) "ERROR" s
-- for testing parseTrue
trueParser :: String -> Either ParseError Expr
trueParser s = parse (do
                    res <- parseTrue
                    eof
                    return res) "ERROR" s

-- for testing parseUndefined
undefinedParser :: String -> Either ParseError Expr
undefinedParser s = parse (do
                    res <- parseUndefined
                    eof
                    return res) "ERROR" s

-- for testing parseAssign
assignParser :: String -> Either ParseError Expr
assignParser s = parse (do
                    res <- parseAssign
                    eof
                    return res) "ERROR" s

-- for testing parseCall
callParser :: String -> Either ParseError Expr
callParser s = parse (do
                    res <- parseCall
                    eof
                    return res) "ERROR" s

-- for testing parseIdent
identParser :: String -> Either ParseError Expr
identParser s = parse (do
                    res <- parseIdent
                    eof
                    return res) "ERROR" s

-- for testing parseArray
arrayParser :: String -> Either ParseError Expr
arrayParser s = parse (do
                    res <- parseArray
                    eof
                    return res) "ERROR" s

-- for testing parseArrayStart
arrayStartParser :: String -> Either ParseError Expr
arrayStartParser s = parse (do
                    res <- parseArrayStart
                    eof
                    return res) "ERROR" s

-- for testing parseParentheses
parenthesesParser :: String -> Either ParseError Expr
parenthesesParser s = parse (do
                    res <- parseParentheses
                    eof
                    return res) "ERROR" s