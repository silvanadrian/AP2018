-- This is a skeleton file for you to edit

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions
import Data.Either

-- Exercise 1.1
-- Helper to make it nicer to print
showExpStr :: Exp -> Exp -> String -> String
showExpStr a b s = "(" ++ showExp a ++ s ++ showExp b ++ ")"

showExp :: Exp -> String
showExp (Cst as) =
  if head(show as) == '-' then "(" ++ show as ++ ")" else show as
showExp (Add a b) = showExpStr a b " + "
showExp (Sub a b) = showExpStr a b " - "
showExp (Mul a b) = showExpStr a b " * "
showExp (Div a b) = showExpStr a b " / "
showExp (Pow a b) = showExpStr a b "^"
showExp _ = error "is not supported"

-- Exercise 1.2
evalSimple :: Exp -> Integer
evalSimple (Cst a) = a
evalSimple (Add a b) = evalSimple a + evalSimple b
evalSimple (Sub a b) = evalSimple a - evalSimple b
evalSimple (Mul a b) = evalSimple a * evalSimple b
-- div checks it self i b is zero
evalSimple (Div a b) = evalSimple a `div` evalSimple b
-- check ourselvs for negative exponent
-- and run a first with seq to se that there is nothing illegal there
evalSimple (Pow a b)
  | evalSimple b < 0 = error "Negative exponent"
  | otherwise = seq (evalSimple a) (evalSimple a ^ evalSimple b)
evalSimple _ = error "is not supported"

-- Exercise 2
extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r a = if v == a then Just n else r a

-- used to check if variable is unbound
intTest :: Maybe Integer -> Integer
intTest (Just i) = i
intTest _ = error "variable is unbound"

-- helper to calculate sum
-- takes integers instead of expressions
summ :: VName -> Integer -> Integer -> Exp -> Env -> Integer
summ v a b c r = if a > b then 0 else
  evalFull c r + summ v (a+1) b c (extendEnv v (a+1) r)

evalFull :: Exp -> Env -> Integer
evalFull (Cst a) _ = a
evalFull (Add a b) r = evalFull a r + evalFull b r
evalFull (Sub a b) r = evalFull a r - evalFull b r
evalFull (Mul a b) r = evalFull a r * evalFull b r
evalFull (Div a b) r = evalFull a r `div` evalFull b r
-- check for negative exponent
evalFull (Pow a b) r
  | evalFull b r < 0 = error "Negative exponent"
  | otherwise = seq (evalFull a r) (evalFull a r ^ evalFull b r)
-- check if a is zero
evalFull (If a b c) r =
  if evalFull a r /= 0 then evalFull b r else evalFull c r
evalFull (Var v) r = intTest(r v)
evalFull (Let a b c) r = evalFull c (extendEnv a (evalFull b r) r)
evalFull (Sum v a b c) r =
  summ v (evalFull a r) (evalFull b r) c (extendEnv v (evalFull a r) r)

-- Exercise 3
intTestErr :: Maybe Integer -> VName -> Either ArithError Integer
intTestErr (Just i) _ = Right i
intTestErr _ v = Left (EBadVar v)

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst a) _ = Right a
evalErr (Add a b) r = evalEither (evalErr a r) (+) (evalErr b r)
evalErr (Sub a b) r = evalEither (evalErr a r) (-) (evalErr b r)
evalErr (Mul a b) r = evalEither (evalErr a r) (*) (evalErr b r)
-- check for division by zero
evalErr (Div a b) r = if isRight (evalErr b r)
                        then if fromRight' (evalErr b r) /= 0
                          then evalEither (evalErr a r) div (evalErr b r)
                          else Left EDivZero
                        else evalErr b r
-- check for negative exponent
evalErr (Pow a b) r = if isRight (evalErr b r)
                        then if fromRight' (evalErr b r) >= 0
                          then evalEither (evalErr a r) (^) (evalErr b r)
                          else Left ENegPower
                        else evalErr b r
-- check if a is zero
evalErr (If a b c) r = if isRight (evalErr a r)
                          then if fromRight' (evalErr a r) /= 0
                            then evalErr b r
                            else evalErr c r
                        else evalErr a r
evalErr (Var v) r = intTestErr (r v) v
evalErr (Let a b c) r = if isRight (evalErr b r)
                          then evalErr c (extendEnv a (fromRight'(evalErr b r)) r)
                          else evalErr b r

evalErr (Sum v a b c) r = if isRight (evalErr a r)
                            then if isRight (evalErr b r)
                              then Right (summ v (fromRight' (evalErr a r)) (fromRight' (evalErr b r)) c (extendEnv v (fromRight'(evalErr a r)) r))
                              else evalErr b r
                            else evalErr a r

evalEither :: Either a b -> (b -> b -> b) -> Either a b -> Either a b
evalEither a b c = if isRight a
                        then if isRight c
                          then Right ( b (fromRight' a) (fromRight' c))
                          else c
                        else a

-- use own implementation of fromRight from Data.Either but not returning a 
-- default value, which is not needed for the assignment
fromRight' :: Either a b -> b
fromRight' (Right c) = c
fromRight' _ = error "No value"                              
  
-- optional parts (if not attempted, leave them unmodified)                    

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
