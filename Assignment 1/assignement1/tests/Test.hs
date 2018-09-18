import Test.Tasty
import Test.Tasty.HUnit

import SubsInterpreter
import SubsAst

main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [
      equalityTests,
      smallerThenTests,
      addTests,
      mulTests,
      subTests,
      moduloTests
    ]


equalityTests :: TestTree
equalityTests = testGroup "equalityTests" 
  [
    testCase "eq IntVal" $ equality [IntVal 1, IntVal 1] @?= Right TrueVal,
    testCase "neq IntVal" $ equality [IntVal 1, IntVal 2] @?= Right FalseVal,
    testCase "eq UndefinedVal" $ equality [UndefinedVal, UndefinedVal]  @?= Right TrueVal,
    testCase "eq StringVal" $ equality [StringVal "abc", StringVal "abc"] @?= Right TrueVal,
    testCase "neq StringVal" $ equality [StringVal "abc", StringVal "ab"]  @?= Right FalseVal,
    testCase "eq TrueVal" $ equality [TrueVal, TrueVal] @?= Right TrueVal,
    testCase "neq TrueVal" $ equality [TrueVal, FalseVal]  @?= Right FalseVal,
    testCase "eq FalseVal" $ equality [FalseVal, FalseVal] @?= Right TrueVal,
    testCase "neq FalseVal" $ equality [FalseVal, TrueVal] @?= Right FalseVal,
    testCase "eq empty ArrayVal" $ equality [ArrayVal [], ArrayVal []] @?= Right TrueVal,
    testCase "eq ArrayVal" $ equality [ArrayVal [IntVal 1, IntVal 2, IntVal 3], ArrayVal [IntVal 1,IntVal 2, IntVal 3]] @?= Right TrueVal,
    testCase "neq ArrayVal size" $ equality [ArrayVal [IntVal 1, IntVal 2], ArrayVal [IntVal 1,IntVal 2, IntVal 3]] @?= Right FalseVal,
    testCase "neq ArrayVal empty" $ equality [ArrayVal [], ArrayVal [IntVal 1,IntVal 2, IntVal 3]] @?= Right FalseVal,
    testCase "wildcard FalseVal" $ equality [FalseVal,IntVal 1] @?= Right FalseVal,
    testCase "wildcard arguments" $ equality [FalseVal] @?= Left "Wrong amount of Arguments"
  ]

smallerThenTests :: TestTree
smallerThenTests = testGroup "smallerThenTests"
 [
   testCase "smaller IntVal true" $ smallerThen [IntVal 1, IntVal 2] @?= Right TrueVal,
   testCase "smaller IntVal false" $ smallerThen [IntVal 3, IntVal 2] @?= Right FalseVal,
   testCase "smaller StringVal true" $ smallerThen [StringVal "abc", StringVal "abz"] @?= Right TrueVal,
   testCase "smaller StringVal false" $ smallerThen [StringVal "abzz", StringVal "abz"] @?= Right FalseVal,
   testCase "wildcard FalseVal" $ smallerThen [FalseVal,IntVal 1] @?= Right FalseVal,
   testCase "wildcard arguments" $ equality [FalseVal] @?= Left "Wrong amount of Arguments"
 ]

addTests :: TestTree
addTests = testGroup "addTests"
 [
   testCase "IntVal add" $ add [IntVal 1, IntVal 99] @=? Right (IntVal 100),
   testCase "StringVal add" $ add [StringVal "a", StringVal "b"] @=? Right (StringVal "ab"),
   testCase "StringVal/IntVal add" $ add [StringVal "a", IntVal 1] @=? Right (StringVal "a1"),
   testCase "StringVal/IntVal add" $ add [IntVal 1, StringVal "a"] @=? Right (StringVal "1a"),
   testCase "wildcard add" $ add  [FalseVal, TrueVal] @=? Left "No Int or String",
   testCase "wildcard arguments" $ add [FalseVal] @?= Left "Wrong amount of Arguments"
 ]

mulTests :: TestTree
mulTests = testGroup "mulTests" 
  [
    testCase "IntVal mul" $ mul [IntVal 1, IntVal 3] @=? Right (IntVal 3),
    testCase "wildcard mul" $ mul [FalseVal, TrueVal] @?= Left "No Integer",
    testCase "wildcard arguments" $ mul [FalseVal] @?= Left "Wrong amount of Arguments"
  ]

subTests :: TestTree
subTests = testGroup "subTests"
  [
    testCase "IntVal sub" $ sub [IntVal 3, IntVal 4] @=? Right (IntVal (-1)),
    testCase "wildcar sub" $ sub [FalseVal, TrueVal] @=? Left "No Integer",
    testCase "wildcar arguments" $ sub [FalseVal] @=? Left "Wrong amount of Arguments"
  ]

moduloTests :: TestTree
moduloTests = testGroup "moduleTests"
  [
    testCase "IntVal modulo" $ modulo [IntVal 4, IntVal 2] @?= Right (IntVal 0),
    testCase "IntVal div 0" $ modulo [IntVal 4, IntVal 0] @?= Left "Division by Zero",
    testCase "wildcar modulo" $ modulo [FalseVal, TrueVal] @=? Left "No Integer",
    testCase "wildcar arguments" $ modulo [FalseVal] @=? Left "Wrong amount of Arguments"
  ]

runExprTests :: TestTree
runExprTests = testGroup "runExprTests"
  [ testCase "intro" $
      runExpr introExpr @?= Right introResult
  , testCase "scope" $
      runExpr scopeExpr @?= Right scopeResult
  ]

introExpr :: Expr
introExpr =
  Comma (Assign "xs"
          (Array [Number 0, Number 1, Number 2, Number 3, Number 4,
                  Number 5, Number 6, Number 7, Number 8, Number 9]))
   (Comma (Assign "squares"
            (Compr (ACFor "x" (Var "xs")
                     (ACBody (Call "*" [Var "x",Var "x"])))))
     (Comma (Assign "evens"
              (Compr (ACFor "x" (Var "xs")
                       (ACIf (Call "===" [Call "%" [Var "x", Number 2],
                                          Number 0])
                         (ACBody (Var "x"))))))
       (Comma (Assign "many_a"
                (Compr (ACFor "x" (Var "xs")
                         (ACFor "y" (Var "xs")
                           (ACBody (String "a"))))))
         (Comma (Assign "hundred"
                  (Compr (ACFor "i" (Array [Number 0])
                           (ACFor "x" (Call "Array" [Number 5])
                             (ACFor "y" (Call "Array" [Number 20])
                               (ACBody (Assign "i"
                                         (Call "+" [Var "i", Number 1]))))))))
           (Array [Var "xs", Var "squares", Var "evens",
                   Var "many_a", Var "hundred"])))))

introResult :: Value
introResult = ArrayVal
  [ ArrayVal [IntVal n | n <- [0..9]]
  , undefined
  , undefined
  , undefined
  , undefined
  ]

scopeExpr :: Expr
scopeExpr =
  Comma (Assign "x" (Number 42))
   (Comma (Assign "y" (Compr (ACFor "x" (String "abc")
                               (ACBody (Var "x")))))
     (Array [Var "x", Var "y"]))

scopeResult :: Value
scopeResult = ArrayVal
  undefined
