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
showExp _ = error "NOOOOOOO"

evalSimple :: Exp -> Integer
--evalSimple (Div (Cst a) (Cst b)) = a div` b
evalSimple = undefined

extendEnv :: VName -> Integer -> Env -> Env
extendEnv = undefined

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
