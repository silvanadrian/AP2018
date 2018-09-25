module SubsParser (
    parseString,
    parseFile
  ) where

import SubsAst
import Parser.Impl
import Text.ParserCombinators.Parsec.Error

-- shouldn't need to change this
parseFile :: FilePath -> IO (Either ParseError Expr)
parseFile path = fmap parseString $ readFile path
