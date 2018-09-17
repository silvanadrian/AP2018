import Test.Tasty
import Test.Tasty.HUnit

import Definitions
import Arithmetic

main = defaultMain tests

tests :: TestTree
tests = testGroup "test" [
    showExpTest,
    evalSimpleTest,
    extendEnvTest,
    evalFullTest,
    evalErrTest
    ]

showExpTest :: TestTree
showExpTest = testGroup "showExp"
  [ testCase "Show Mul" $ showExp (Mul (Cst 2) (Cst 3)) @?= "(2 * 3)",
    testCase "Show Minus" $ showExp(Cst (-3)) @?= "(-3)",
    testCase "Show Add" $ showExp(Add (Cst (-3)) (Cst (-4))) @?= "((-3) + (-4))",
    testCase "Show Pow" $ showExp(Pow (Cst 2) (Cst 3)) @?= "(2^3)",
    testCase "Show Div" $ showExp(Div (Cst 3) (Cst 4)) @?= "(3 / 4)",
    testCase "Show Sub" $ showExp(Sub (Cst 2) (Cst 4)) @?= "(2 - 4)",
    testCase "Show Mixture" $ showExp(Pow (Div (Cst 2) (Cst 3)) (Sub (Cst 4) (Cst 3))) @=? "((2 / 3)^(4 - 3))" 
  ]

evalSimpleTest :: TestTree
evalSimpleTest = testGroup "evalSimple"
  [ testCase "Cst" $ evalSimple (Cst 3) @?= 3,
    testCase "Add" $ evalSimple(Add (Cst 3) (Cst 5)) @?= 8,
    testCase "Sub" $ evalSimple(Sub (Cst 3) (Cst 5)) @?= -2,
    testCase "Mul" $ evalSimple(Mul (Cst 3) (Cst 5)) @?= 15,
    testCase "Div" $ evalSimple(Div (Cst 12) (Cst 3)) @?= 4,
    testCase "Pow" $ evalSimple(Pow (Cst 2) (Cst 3)) @?= 8,
    testCase "Mixture" $ evalSimple(Pow (Div (Cst 2) (Cst 3)) (Sub (Cst 4) (Cst 3))) @=? 0
  ]

extendEnvTest :: TestTree
extendEnvTest = testGroup "extendEnv"
  [
      testCase "extendEnv Simple" $ (extendEnv "x" 5 initEnv) "x" @?= Just 5,
      testCase "extendEnv Extended" $ (extendEnv "x" 5 (extendEnv "x" 6 initEnv)) "x" @=? Just 5
  ]

evalFullTest :: TestTree
evalFullTest = testGroup "fullEval"
  [
    testCase "Cst" $ evalFull(Cst 3) initEnv @?= 3,
    testCase "Add" $ evalFull(Add (Cst 3) (Cst 5)) initEnv @=? 8,
    testCase "Sub" $ evalFull(Sub (Cst 3) (Cst 5)) initEnv @?= -2,
    testCase "Mul" $ evalFull(Mul (Cst 3) (Cst 5)) initEnv @?= 15,
    testCase "Div" $ evalFull(Div (Cst 12) (Cst 3)) initEnv @?= 4,
    testCase "Pow" $ evalFull(Pow (Cst 2) (Cst 3)) initEnv @?= 8,
    testCase "Let" $ evalFull(Let "z" (Add (Cst 2) (Cst 3)) (Var "z")) initEnv @?= 5,
    testCase "Sum" $ evalFull(Sum "x" (Cst 10) (Add (Cst 5) (Cst 5)) (Mul (Cst 3) (Var "x"))) initEnv @=? 30
  ]

evalErrTest :: TestTree
evalErrTest = testGroup "errEval"
  [
      testCase "Cst" $ evalErr (Cst 3) initEnv @?= Right 3,
      testCase "Add" $ evalErr(Add (Cst 3) (Cst 5)) initEnv @=? Right 8,
      testCase "Sub" $ evalErr(Sub (Cst 3) (Cst 5)) initEnv @?= Right (-2),
      testCase "Mul" $ evalErr(Mul (Cst 3) (Cst 5)) initEnv @?= Right 15,
      testCase "Div" $ evalErr(Div (Cst 12) (Cst 3)) initEnv @?= Right 4,
      testCase "Pow" $ evalErr(Pow (Cst 2) (Cst 3)) initEnv @?= Right 8,
      testCase "Let" $ evalErr(Let "z" (Add (Cst 2) (Cst 3)) (Var "z")) initEnv @?= Right 5,
      testCase "Sum" $ evalErr(Sum "x" (Cst 10) (Add (Cst 5) (Cst 5)) (Mul (Cst 3) (Var "x"))) initEnv @=? Right 30,
      testCase "Div 0" $ evalErr(Div (Cst 12) (Cst 0)) initEnv @?= Left EDivZero,
      testCase "Pow Negative" $ evalErr (Pow (Cst 2) (Cst (-1))) initEnv @?= Left ENegPower
  ]