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
    , parseStartArrayTests
    , parseParanthesTests
    , parseExprs
    , parseComma
    , parseExprTests
    , parseArrayCompr
    , parseSimpleExprTests
    , parseComplexExprTests
    , predefinedTests
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
      stringParser ("'abc\n\t'") @?= Right (String "abc")
    , testCase "String  not allowed special char" $
      show(stringParser ("'\a'")) @?= ""
    , testCase "String whitespaced" $
      stringParser ("'asdas asdasd'") @?= Right (String "asdas asdasd")
    , testCase "String newline" $
      stringParser ("'foo \\\n bar'") @?= Right (String "foobar")
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
parseAssignTests =
  testGroup
    "Assign"
    [ testCase "Assign" $ assignParser ("x=3") @?= Right (Assign "x" (Number 3))
    , testCase "Assign whitespace/special char" $
      assignParser ("x = \n 3") @?= Right (Assign "x" (Number 3))
    , testCase "Assign underline" $
      assignParser ("x_x=0") @?= Right (Assign "x_x" (Number 0))
    ]

parseCallTests :: TestTree
parseCallTests =
  testGroup
    "Call"
    [ testCase "Call" $ callParser ("x(12)") @?= Right (Call "x" [Number 12])
    , testCase "Call whitespace" $
      callParser ("x ( 12 ) ") @?= Right (Call "x" [Number 12])
    ]

parseIdentTests :: TestTree
parseIdentTests =
  testGroup
    "Ident"
    [ testCase "Ident" $ identParser ("x_x") @?= Right (Var "x_x")
    , testCase "Ident similar to keyword" $
      identParser ("falsee") @?= Right (Var "falsee")
    , testCase "Ident keyword" $
      show (identParser ("false")) @?=
      "Left \"ERROR\" (line 1, column 6):\nunexpected end of input\nexpecting digit, letter or \"_\"\nshould not be a keyword"
    , testCase "Ident whitespace" $
      show (identParser ("x_x    ")) @?=
      "Left \"ERROR\" (line 1, column 4):\nunexpected ' '\nexpecting digit, letter, \"_\" or end of input"
    ]

parseArrayTests :: TestTree
parseArrayTests =
  testGroup
    "Array"
    [ testCase "Array" $
      parseString ("[1,2]") @?= Right (Array [Number 1, Number 2])
    , testCase "Array whitespace" $
      parseString ("[ 1,  'sds']   ") @?= Right (Array [Number 1, String "sds"])
    ]

parseStartArrayTests :: TestTree
parseStartArrayTests =
  testGroup
    "Array Compr"
    [ testCase "Array for" $
      parseString ("[for (x of 2) 2]") @?=
      Right (Compr (ACFor "x" (Number 2) (ACBody (Number 2))))
    ]

parseParanthesTests :: TestTree
parseParanthesTests =
  testGroup
    "Parantheses"
    [ testCase "Parantheses" $ parseString ("(1)") @?= Right (Number 1)
    , testCase "Parantheses whitespace" $
      parseString ("(   1    )") @?= Right (Number 1)
    ]

parseExprs :: TestTree
parseExprs =
  testGroup
    "parseExprs"
    [ testCase "parseExprs numbers" $
      parseString ("[1,2,3]") @?= Right (Array [Number 1, Number 2, Number 3])
    , testCase "parseExprs" $
      parseString ("['a','b','c']") @?=
      Right (Array [String "a", String "b", String "c"])
    , testCase "parseExprs ident" $
      parseString ("a (1,2,3)") @?=
      Right (Call "a" [Number 1, Number 2, Number 3])
    ]

parseComma :: TestTree
parseComma =
  testGroup
    "Comma"
    [ testCase "Parse Comma" $
      parseString ("1,2") @=? Right (Comma (Number 1) (Number 2))
    , testCase "Parse nested commas" $
      parseString ("1,(1,(3,4))") @?=
      Right (Comma (Number 1) (Comma (Number 1) (Comma (Number 3) (Number 4))))
    , testCase "many commas" $
      parseString ("1,2,3,'a','b'") @?=
      Right
        (Comma
           (Number 1)
           (Comma
              (Number 2)
              (Comma (Number 3) (Comma (String "a") (String "b")))))
    ]

parseExprTests :: TestTree
parseExprTests =
  testGroup
    "parseExpr"
    [ testCase "Additon" $
      parseString ("1+1") @=? Right (Call "+" [Number 1, Number 1])
    , testCase "Subtraction" $
      parseString ("1-1") @?= Right (Call "-" [Number 1, Number 1])
    , testCase "Mul" $
      parseString ("1*1") @?= Right (Call "*" [Number 1, Number 1])
    , testCase "Mod" $
      parseString ("1%1") @?= Right (Call "%" [Number 1, Number 1])
    , testCase "Smaller Then" $
      parseString ("1<1") @?= Right (Call "<" [Number 1, Number 1])
    , testCase "Equals" $
      parseString ("1===1") @?= Right (Call "===" [Number 1, Number 1])
    ]

parseArrayCompr :: TestTree
parseArrayCompr =
  testGroup
    "Array Compr"
    [ testCase "for" $
      parseString ("[for (x of 2) 3]") @=?
      Right (Compr (ACFor "x" (Number 2) (ACBody (Number 3))))
    , testCase "nested for" $
      parseString ("[for (x of 2) for (x of 3) 3]") @=?
      Right
        (Compr (ACFor "x" (Number 2) (ACFor "x" (Number 3) (ACBody (Number 3)))))
    , testCase "nested if" $
      parseString ("[for (x of 2) if(1) 2]") @=?
      Right (Compr (ACFor "x" (Number 2) (ACIf (Number 1) (ACBody (Number 2)))))
    , testCase "mixed for/if" $
      parseString ("[for (x of 2) if(1) for (y of 2) if(false) for(z of 5) 2]") @=?
      Right
        (Compr
           (ACFor
              "x"
              (Number 2)
              (ACIf
                 (Number 1)
                 (ACFor
                    "y"
                    (Number 2)
                    (ACIf FalseConst (ACFor "z" (Number 5) (ACBody (Number 2))))))))
    ]

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

parseSimpleExprTests :: TestTree
parseSimpleExprTests =
  testGroup
    "Simple expr tests"
    [ testCase "equal" $
      parseString ("a===b===c") @?=
      Right (Call "===" [Call "===" [Var "a", Var "b"], Var "c"])
    , testCase "assign" $
      parseString ("a=b=undefined") @?=
      Right (Assign "a" (Assign "b" Undefined))
    , testCase "smallerThen" $
      parseString ("2<3<4") @?=
      Right (Call "<" [Call "<" [Number 2, Number 3], Number 4]),
      testCase "whitespace" $ parseString("12   \v \t\t     \n") @?= Right (Number 12),
      testCase "comment" $ parseString("1 //comment 11212121212\n,2") @?= Right (Comma (Number 1) (Number 2)),
      testCase "comment at start" $ parseString("//comment \n 2   ") @?= Right (Number 2)
    ]


parseComplexExprTests :: TestTree
parseComplexExprTests = testGroup "Complex expr tests"
  [
    testCase "scope.js" $ parseString("x = 42, y = [for (x of 'abc') x],[x, y]") @?= Right (Comma (Assign "x" (Number 42)) (Comma (Assign "y" (Compr (ACFor "x" (String "abc") (ACBody (Var "x"))))) (Array [Var "x",Var "y"])))
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
