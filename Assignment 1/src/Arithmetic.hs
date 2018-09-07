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
showExp (Cst as) = show as
showExp (Add a b) = "(" ++ (showExp (a)) ++ " + " ++ (showExp (b)) ++ ")"
showExp (Sub a b) = "(" ++ (showExp (a)) ++ " - " ++ (showExp (b)) ++ ")"
showExp (Mul a b) = "(" ++ (showExp (a)) ++ " * " ++ (showExp (b)) ++ ")"
showExp (Div a b) = "(" ++ (showExp (a)) ++ " / " ++ (showExp (b)) ++ ")"
showExp (Pow a b) = "(" ++ (showExp (a)) ++ "^" ++ (showExp (b)) ++ ")"
showExp _ = error "is not supported"

evalSimple :: Exp -> Integer
evalSimple (Cst a) = a
evalSimple (Add a b) = evalSimple(a) + evalSimple(b)
evalSimple (Sub a b) = evalSimple(a) - evalSimple(b)
evalSimple (Mul a b) = evalSimple(a) * evalSimple(b)
evalSimple (Div a b) = evalSimple(a) `div` evalSimple(b)
evalSimple (Pow a b) = evalSimple(a) ^ evalSimple(b)
evalSimple _ = error "is not supported"

extendEnv :: VName -> Integer -> Env -> Env
extendEnv = undefined
extendEnv v n r =  \a -> if v == a then Just n else r a

evalFull :: Exp -> Env -> Integer
evalFull = undefined

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr = undefined

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
