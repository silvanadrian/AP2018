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

evalSimple :: Exp -> Integer
evalSimple (Cst a) = a
evalSimple (Add a b) = evalSimple a + evalSimple b
evalSimple (Sub a b) = evalSimple a - evalSimple b
evalSimple (Mul a b) = evalSimple a * evalSimple b
evalSimple (Div a b) = evalSimple a `div` evalSimple b
evalSimple (Pow a b)
  | evalSimple b < 0 = error "Negative exponent"
  | evalSimple b == 0 = 1
  | otherwise = evalSimple a * evalSimple(Pow a (Sub b (Cst 1)))
evalSimple _ = error "is not supported"
-- evalSimple (Pow a b) = evalSimple a ^ evalSimple b

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r a = if v == a then Just n else r a

intTest :: Maybe Integer -> Integer
intTest (Just i) = i
intTest _ = error "Value is unbound"

summ :: VName -> Integer -> Integer -> Exp -> Env -> Integer
summ v a b c r = if a > b then 0 else
  evalFull c r + summ v (a+1) b c (extendEnv v (a+1) r)

evalFull :: Exp -> Env -> Integer
evalFull (Cst a) _ = a
evalFull (Add a b) r = evalFull a r + evalFull b r
evalFull (Sub a b) r = evalFull a r - evalFull b r
evalFull (Mul a b) r = evalFull a r * evalFull b r
evalFull (Div a b) r = evalFull a r `div` evalFull b r
evalFull (Pow a b) r = evalFull a r ^ evalFull b r
evalFull (If a b c) r =
  if evalFull a r /= 0 then evalFull b r else evalFull c r
evalFull (Var v) r = intTest(r v)
evalFull (Let a b c) r = evalFull c (extendEnv a (evalFull b r) r)
evalFull (Sum v a b c) r =
  summ v (evalFull a r) (evalFull b r) c (extendEnv v (evalFull a r) r)

-- summ' :: VName -> Integer -> Integer -> Exp -> Env -> Integer
-- summ' v a b c r = if a > b then Right 0 else
--   Right (evalErr c r) + Right (summ' v (a+1) b c (extendEnv v (a+1) r))

intTestErr :: Maybe Integer -> VName -> Either ArithError Integer
intTestErr (Just i) _ = Right i
intTestErr _ v = Left (EBadVar v)

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst a) _ = Right a
evalErr (Add a b) r = evalEither (evalErr a r) (+) (evalErr b r)
evalErr (Sub a b) r = evalEither (evalErr a r) (-) (evalErr b r)
evalErr (Mul a b) r = evalEither (evalErr a r) (*) (evalErr b r)
evalErr (Div a b) r = if isRight (evalErr b r)
                        then if fromRight' (evalErr b r) /= 0
                          then evalEither (evalErr a r) div (evalErr b r)
                          else Left EDivZero
                        else evalErr b r
evalErr (Pow a b) r = if isRight (evalErr b r)
                        then if fromRight' (evalErr b r) >= 0
                          then evalEither (evalErr a r) (^) (evalErr b r)
                          else Left ENegPower
                        else evalErr b r
evalErr (If a b c) r = if isRight (evalErr a r)
                          then if fromRight' (evalErr a r) /= 0
                            then evalErr b r
                            else evalErr c r
                        else (evalErr a r)
evalErr (Var v) r = intTestErr (r v) v
evalErr (Let a b c) r = if isRight (evalErr b r)
                          then evalErr c (extendEnv a (fromRight'(evalErr b r)) r)
                          else evalErr b r

-- evalErr (Sum v a b c) r =
--  Right (summ v (makeInt(evalErr a r)) (makeInt(evalErr b r)) c (extendEnvErr v (evalErr a r) r))

-- optional parts (if not attempted, leave them unmodified)

evalEither :: Either a i -> (i -> i -> i) -> Either a i -> Either a i
evalEither a b c = if isRight a
                        then if isRight c
                          then Right ( b (fromRight' a) (fromRight' c))
                          else c
                        else a

fromRight' :: Either a b -> b
fromRight' (Right c) = c
fromRight' _ = error ("No value")                       

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
