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

showExp :: Exp -> String
showExp (Cst as) = if head(show as) == '-' then "(" ++ show as ++ ")" else show as
showExp (Add a b) = "(" ++ showExp a ++ " + " ++ showExp b ++ ")"
showExp (Sub a b) = "(" ++ showExp a ++ " - " ++ showExp b ++ ")"
showExp (Mul a b) = "(" ++ showExp a ++ " * " ++ showExp b ++ ")"
showExp (Div a b) = "(" ++ showExp a ++ " / " ++ showExp b ++ ")"
showExp (Pow a b) = "(" ++ showExp a ++ "^" ++ showExp b ++ ")"
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

summ :: VName -> Integer -> Integer -> Integer -> Env -> Integer
summ v a b c r = if intTest(r v) > a then c else summ v a b (b+b) (extendEnv v (intTest(r v)+1) r)

evalFull :: Exp -> Env -> Integer
evalFull (Cst a) _ = a
evalFull (Add a b) r = evalFull a r + evalFull b r
evalFull (Sub a b) r = evalFull a r - evalFull b r
evalFull (Mul a b) r = evalFull a r * evalFull b r
evalFull (Div a b) r = evalFull a r `div` evalFull b r
evalFull (Pow a b) r = evalFull a r ^ evalFull b r

evalFull (If a b c) r = if evalFull a r /= 0 then evalFull b r else evalFull c r
evalFull (Var v) r = intTest(r v)
evalFull (Let a b c) r = evalFull c (extendEnv a (evalFull b r) r)
evalFull (Sum v a b c) r = summ v (evalFull b r) (evalFull c r) 0 (extendEnv v (evalFull a r) r)

-- if intTest(b r) > (evalFull c r) then 0 else
-- extendEnv a ((evalFull b r) + 1) r (evalFull d r + evalFull (Sum a ))
-- (evalFull d r + (evalFull (Sum a (b+1) c d) r))

-- evalFull _ _ = error "is not supported"
-- evalFull (Var v) r = if r v == Nothing then error "HHH" else r v --Maybe integer

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr = undefined

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
