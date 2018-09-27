import Test.Tasty
import Test.Tasty.HUnit

import SubsInterpreter
import SubsAst
import qualified Data.Map as Map

main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [
      equalityTests,
      smallerThenTests,
      addTests,
      mulTests,
      subTests,
      moduloTests,
      arrayTests,
      runExprTests,
      monadLawsTest
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
    testCase "wildcard arguments" $ equality [FalseVal] @?= Left "Wrong number of arguments"
  ]

smallerThenTests :: TestTree
smallerThenTests = testGroup "smallerThenTests"
 [
   testCase "smaller IntVal true" $ smallerThen [IntVal 1, IntVal 2] @?= Right TrueVal,
   testCase "smaller IntVal false" $ smallerThen [IntVal 3, IntVal 2] @?= Right FalseVal,
   testCase "smaller StringVal true" $ smallerThen [StringVal "abc", StringVal "abz"] @?= Right TrueVal,
   testCase "smaller StringVal false" $ smallerThen [StringVal "abzz", StringVal "abz"] @?= Right FalseVal,
   testCase "wildcard FalseVal" $ smallerThen [FalseVal,IntVal 1] @?= Right FalseVal,
   testCase "wildcard arguments" $ equality [FalseVal] @?= Left "Wrong number of arguments"
 ]

addTests :: TestTree
addTests = testGroup "addTests"
 [
   testCase "IntVal add" $ add [IntVal 1, IntVal 99] @?= Right (IntVal 100),
   testCase "StringVal add" $ add [StringVal "a", StringVal "b"] @?= Right (StringVal "ab"),
   testCase "StringVal/IntVal add" $ add [StringVal "a", IntVal 1] @?= Right (StringVal "a1"),
   testCase "StringVal/IntVal add" $ add [IntVal 1, StringVal "a"] @?= Right (StringVal "1a"),
   testCase "wildcard add" $ add  [FalseVal, TrueVal] @?= Left "No Int or String",
   testCase "wildcard arguments" $ add [FalseVal] @?= Left "Wrong number of arguments"
 ]

mulTests :: TestTree
mulTests = testGroup "mulTests" 
  [
    testCase "IntVal mul" $ mul [IntVal 1, IntVal 3] @?= Right (IntVal 3),
    testCase "wildcard mul" $ mul [FalseVal, TrueVal] @?= Left "No Integer",
    testCase "wildcard arguments" $ mul [FalseVal] @?= Left "Wrong number of arguments"
  ]

subTests :: TestTree
subTests = testGroup "subTests"
  [
    testCase "IntVal sub" $ sub [IntVal 3, IntVal 4] @?= Right (IntVal (-1)),
    testCase "wildcar sub" $ sub [FalseVal, TrueVal] @?= Left "No Integer",
    testCase "wildcar arguments" $ sub [FalseVal] @?= Left "Wrong number of arguments"
  ]

moduloTests :: TestTree
moduloTests = testGroup "moduleTests"
  [
    testCase "IntVal modulo" $ modulo [IntVal 4, IntVal 2] @?= Right (IntVal 0),
    testCase "IntVal div 0" $ modulo [IntVal 4, IntVal 0] @?= Left "Division by Zero",
    testCase "wildcar modulo" $ modulo [FalseVal, TrueVal] @?= Left "No Integer",
    testCase "wildcar arguments" $ modulo [FalseVal] @?= Left "Wrong number of arguments"
  ]

arrayTests :: TestTree
arrayTests = testGroup "arrayTests"
 [
   testCase "mkArray n size" $ mkArray [IntVal 4] @?= Right (ArrayVal [UndefinedVal,UndefinedVal,UndefinedVal,UndefinedVal]),
   testCase "mkArray empty" $ mkArray [] @?= Left "Array() called with wrong number or type of arguments"
 ] 

runExprTests :: TestTree
runExprTests = testGroup "runExprTests"
  [ 
    testCase "undefined" $ runExpr (Undefined) @?= Right UndefinedVal,
    testCase "True" $ runExpr (TrueConst) @?= Right TrueVal,
    testCase "False" $ runExpr (FalseConst) @?= Right FalseVal,
    testCase "Number" $ runExpr (Number 1) @?= Right (IntVal 1),
    testCase "String" $ runExpr (String "123") @?= Right (StringVal "123"),
    testCase "Var" $ runExpr (Var "xs") @?= Left ("No value found in map"),
    testCase "Assign" $ runExpr (Assign "xs" (Number 1)) @?= Right (IntVal 1),
    testCase "Array empty" $ runExpr (Array []) @?= Right (ArrayVal []),
    testCase "Array" $ runExpr (Array [Number 1, Number 1]) @?= Right (ArrayVal [IntVal 1,IntVal 1]),
    testCase "Call equals False" $ runExpr (Call "===" [(Number 1),(Number 2)]) @?= Right FalseVal,
    testCase "Call equals True" $ runExpr (Call "===" [(Number 1),(Number 1)]) @?= Right TrueVal,
    testCase "Call equals false types" $ runExpr (Call "===" [(Number 1),(TrueConst)]) @?= Right FalseVal,
    testCase "Call equals arguments" $ runExpr (Call "===" [(Number 1)]) @?= Left "Wrong number of arguments",
    testCase "Call smallerThen False" $ runExpr (Call "<" [(String "abz"),(String "abc")]) @?= Right FalseVal,
    testCase "Call smallerThen True" $ runExpr (Call "<" [(String "abc"),(String "abz")]) @?= Right TrueVal,
    testCase "Call smallerThen false types" $ runExpr (Call "<" [(String "abc"),(Number 1)]) @?= Right FalseVal,
    testCase "Call smallerThen false types" $ runExpr (Call "<" [(String "abc")]) @?= Left "Wrong number of arguments",
    testCase "Call add Number" $ runExpr (Call "+" [(Number 1),(Number 2)]) @?= Right (IntVal 3),
    testCase "Call add String" $ runExpr (Call "+" [(String "a"),(String "b")]) @?= Right (StringVal "ab"),
    testCase "Call add Number/String" $ runExpr (Call "+" [(Number 1),(String "2")]) @?= Right (StringVal "12"),
    testCase "Call add types" $ runExpr (Call "+" [(FalseConst),(TrueConst)]) @?= Left "No Int or String",
    testCase "Call add arguments" $ runExpr (Call "+" [(FalseConst)]) @?= Left "Wrong number of arguments",
    testCase "Call mul numbers" $ runExpr (Call "*" [(Number 1), (Number 3)]) @?= Right (IntVal 3),
    testCase "Call mul types" $ runExpr (Call "*" [(Number 1), (String "3")]) @?= Left "No Integer",
    testCase "Call mul arguments" $ runExpr (Call "*" [(Number 1)]) @?= Left "Wrong number of arguments",
    testCase "Call sub numbers" $ runExpr (Call "-" [(Number 3),(Number 2)]) @?= Right (IntVal 1),
    testCase "Call sub types" $ runExpr (Call "-" [(Number 3),FalseConst]) @?= Left "No Integer",
    testCase "Call sub arguments" $ runExpr (Call "-" [(Number 3)]) @?= Left "Wrong number of arguments",
    testCase "Call modulo" $ runExpr (Call "%" [(Number 3),(Number 2)]) @?= Right (IntVal 1),
    testCase "Call modulo div 0" $ runExpr (Call "%" [(Number 3),(Number 0)]) @?= Left "Division by Zero",
    testCase "Call modulo types" $ runExpr (Call "%" [(Number 3),(String "1")]) @?= Left "No Integer",
    testCase "Call modulo arguments" $ runExpr (Call "%" [(Number 3)]) @?= Left "Wrong number of arguments",
    testCase "Call array" $ runExpr (Call "Array" [Number 3]) @?= Right (ArrayVal [UndefinedVal,UndefinedVal,UndefinedVal]),
    testCase "Call array arguments" $  runExpr (Call "Array" []) @?= Left "Array() called with wrong number or type of arguments",
    testCase "Comma" $ runExpr(Comma (Number 1) (Number 2)) @?= Right (IntVal 2),
    testCase "ACBody" $ runExpr(Compr(ACBody (Number 1))) @?= Right (IntVal 1),
    testCase "ACIf false" $ runExpr(Compr (ACIf (Call "===" [Call "%" [Number 1, Number 2], Number 0]) (ACBody (Number 2)))) @?= Right (ArrayVal []),
    testCase "ACIf true" $ runExpr(Compr (ACIf (Call "===" [Call "%" [Number 2, Number 2], Number 0]) (ACBody (Number 2)))) @?= Right (IntVal 2),
    testCase "ACFor number" $ runExpr (Compr (ACFor "y" (Array [Number 0, Number 1, Number 2, Number 3]) (ACBody (String "a")))) @?= Right (ArrayVal [StringVal "a",StringVal "a",StringVal "a",StringVal "a"]),
    -- Not Working Test Cases
    -- ACFor for StringVal not Working yet
    testCase "ACFor StringVal" $ runExpr (Compr (ACFor "y" (String "bcd") (ACBody (String "aggg")))) @?= Right (ArrayVal [StringVal "aggg", StringVal "aggg", StringVal "aggg"]),
    -- ACIf not working in a ACFor yet
    testCase "ACIf in ACFor" $ runExpr(Compr (ACFor "x" (Array[Number 1, Number 2]) (ACIf (Call "===" [Call "%" [Number 1, Number 2], Number 0]) (ACBody (Number 3))))) @?= Right (ArrayVal []),
    -- Nested ACFor not working yet
    testCase "ACFor nested" $ runExpr(Comma (Assign "xs" (Array [Number 1,Number 2,Number 3])) (Compr (ACFor "x" (Var "xs") (ACFor "y" (Var "xs") (ACBody (Number 0)))))) @?= Right (ArrayVal [IntVal 0,IntVal 0,IntVal 0,IntVal 0,IntVal 0,IntVal 0,IntVal 0,IntVal 0,IntVal 0]),
    -- Comprehension Scope not working yet
    testCase "Compr scope" $ runExpr(  Comma (Assign "x" (Number 1)) (Comma (Compr (ACFor "x" (Array [Number 2,Number 3]) (ACBody (Var "x")))) (Var "x"))) @=? Right (IntVal 1),
    testCase "intro" $ runExpr introExpr @?= Right introResult,
    testCase "scope" $ runExpr scopeExpr @?= Right scopeResult
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
           (Array [Var "xs", Var "squares", Var "evens",Var "many_a", Var "hundred"])))))

introResult :: Value
introResult = ArrayVal
  [ ArrayVal [IntVal n | n <- [0..9]]
  , ArrayVal [IntVal (n * n) | n <- [0..9] ]
  , ArrayVal [IntVal n | n <- [0,2..8] ]
  , ArrayVal (replicate 100 (StringVal "a"))
  , ArrayVal $ map IntVal $ [1 .. 100]
  ]

scopeExpr :: Expr
scopeExpr =
  Comma (Assign "x" (Number 42))
   (Comma (Assign "y" (Compr (ACFor "x" (String "abc")
                               (ACBody (Var "x")))))
     (Array [Var "x", Var "y"]))

scopeResult :: Value
scopeResult = ArrayVal [ IntVal 42, ArrayVal [ StringVal "a", StringVal "b", StringVal "c" ]]


-- The 3 Monad Laws

-- 1. return v >>= f == f v
actual1Law = return (String "a") >>= runExpr
expected1Law = runExpr $ String "a"

-- 2. m >>= (\a -> return a) == m
actual2Law :: SubsM Expr
actual2Law = return (String "a") >>= return

expected2Law :: SubsM Expr
expected2Law = return $ String "a"

-- 3. (m >>= f) >>= g == m >>= (\a -> (f a >>= g))
-- function g
g (StringVal n) = return $ String "a"
g _ = error "Only StringVal"

actual3Law = (return (String "a") >>= runExpr) >>= g
expected3Law = return (String "a") >>= (\a -> (runExpr a >>= g))


monadLawsTest :: TestTree
monadLawsTest = testGroup "Monads"
    [
        -- first law
        testCase "first Monad Law" $ actual1Law @?= expected1Law,
        -- Second Law, can't run since SubsM doesn't derive from Eq
        -- testCase "second Monad Law" $ actual2Law @?= expected2Law,
        testCase "third Monad Law" $ actual3Law @?= expected3Law
    ]
