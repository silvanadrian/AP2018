-- put your tests here, and/or in other files in the tests/ directory
import Test.Tasty
import Test.Tasty.HUnit

import ParserUtils
import SubsAst
import SubsParser
import Text.ParserCombinators.Parsec.Error

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    -- predefinedTests
    [ constantTests
    , parseNumberTests
    , parseStringTests
    , parseFalseTests
    , parseTrueTests
    , parseUndefinedTests
    , parseAssignTests
    , parseCallTests
    , parseIdentTests
    , parseArrayTests
    ]

parseNumberTests :: TestTree
parseNumberTests =
  testGroup
    "parse number"
    [ testCase "Number pos" $ numberParser ("1") @?= Right (Number 1)
    , testCase "Number neg" $ numberParser ("-2") @?= Right (Number (-2))
    , testCase "Number trailing whitespace" $
      numberParser ("1     ") @?= Right (Number 1)
    , testCase "Number 8 long pos" $
      numberParser ("12345678") @?= Right (Number 12345678)
    , testCase "Number 8 long neg" $
      numberParser ("-12345678") @?= Right (Number (-12345678))
    , testCase "Number too long pos" $
      show (numberParser ("123456789")) @?=
      "Left \"ERROR\" (line 1, column 10):\nunexpected end of input\nexpecting digit\nNumber too long"
    , testCase "Number too long neg" $
      show (numberParser ("-123456789")) @?=
      "Left \"ERROR\" (line 1, column 11):\nunexpected end of input\nexpecting digit\nNumber too long"
    ]

parseStringTests :: TestTree
parseStringTests =
  testGroup
    "parse string"
    [ testCase "String" $ stringParser ("'abc'") @?= Right (String "abc")
    , testCase "String alphaNum" $
      stringParser ("'abc123'") @?= Right (String "abc123")
    , testCase "String allowed special chars" $
      stringParser ("'abc\n\t") @?= Right (String "abc")
    , testCase "String  not allowed special char" $
      stringParser ("'\a'") @?= Right (String "Error")
    , testCase "String whitespaced" $
      stringParser ("'asdas asdasd'") @?= Right (String "asdas asdasd")
    , testCase "String newline" $
      stringParser ("'foo\\\nbar'") @?= Right (String "foobar")
    ]

parseFalseTests :: TestTree
parseFalseTests =
  testGroup
    "parse false"
    [ testCase "False" $ falseParser ("false") @?= Right (FalseConst)
    , testCase "False fail" $
      show (falseParser ("true")) @?=
      "Left \"ERROR\" (line 1, column 1):\nunexpected \"t\"\nexpecting \"false\""
    ]

parseTrueTests :: TestTree
parseTrueTests =
  testGroup
    "parse true"
    [ testCase "True" $ trueParser ("true") @?= Right (TrueConst)
    , testCase "True fail" $
      show (trueParser ("false")) @?=
      "Left \"ERROR\" (line 1, column 1):\nunexpected \"f\"\nexpecting \"true\""
    ]

parseUndefinedTests :: TestTree
parseUndefinedTests =
  testGroup
    "Undefined"
    [ testCase "Undefined" $ undefinedParser ("undefined") @?= Right (Undefined)
    , testCase "Undefined fail" $
      show (undefinedParser ("defined")) @?=
      "Left \"ERROR\" (line 1, column 1):\nunexpected \"d\"\nexpecting \"undefined\""
    ]

parseAssignTests :: TestTree
parseAssignTests = testGroup "Assign"
  [
    testCase "Assign" $ assignParser("x=3") @?= Right (Assign "x" (Number 3)),
    testCase "Assign whitespace/special char" $ assignParser("x = \n 3") @?= Right (Assign "x" (Number 3)),
    testCase "Assign underline" $ assignParser("x_x=0") @?= Right (Assign "x_x" (Number 0))
  ]

parseCallTests :: TestTree
parseCallTests = testGroup "Call"
  [
    testCase "Call" $ callParser("x(12)") @?= Right (Call "x" [Number 12]),
    testCase "Call whitespace" $ callParser("x ( 12 ) ") @?= Right (Call "x" [Number 12])
  ]

parseIdentTests :: TestTree
parseIdentTests = testGroup "Ident"
  [
    testCase "Ident" $ identParser("x_x") @?= Right (Var "x_x"),
    testCase "Ident keyword" $ identParser("falsee") @?= Right (Var "falsee"),
    testCase "Ident whitespace" $ show(identParser("x_x    ")) @?= "Left \"ERROR\" (line 1, column 4):\nunexpected ' '\nexpecting digit, letter, \"_\" or end of input"
  ]

parseArrayTests :: TestTree
parseArrayTests = testGroup "Array"
  [
    testCase "Array" $ parseArray("[1,2]") @?= Right (Array [Number 1,Number 2]),
    testCase "Array whitespace" $ parseArray("[ 1,  'sds']   ") @?= Right (Array [Number 1,String "sds"])
  ]


{-parseStartArrayTests :: TestTree
parseStartArrayTests = testGroup "Array Compr"
  [
    testCase "Array for" $ parseArrayStart("[1,2]") @?= Right (Array [Number 1,Number 2]),
    testCase "Array whitespace" $ parseArrayStart("[ 1,  'sds']   ") @?= Right (Array [Number 1,String "sds"])
  ]-}

constantTests :: TestTree
constantTests =
  testGroup
    "constants tests"
    [ testCase "Number" $ parseString ("2") @?= Right (Number 2)
    , testCase "String" $ parseString ("'abc'") @?= Right (String "abc")
    , testCase "true" $ parseString ("true") @?= Right (TrueConst)
    , testCase "false" $ parseString ("false") @?= Right (FalseConst)
    , testCase "Undefined" $ parseString ("undefined") @?= Right (Undefined)
    , testCase "Ident" $ parseString ("sdsd") @?= Right (Var "sdsd")
    ]

predefinedTests :: TestTree
predefinedTests =
  testGroup
    "predefined tests"
    [ testCase "tiny" $
      parseString "2+3" @?= Right (Call "+" [Number 2, Number 3])
    , testCase "intro" $ do
        act <- parseFile "examples/intro.js"
        exp <- fmap read $ readFile "examples/intro-ast.txt"
        act @?= Right exp
    ]
