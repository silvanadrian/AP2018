module SubsInterpreter
       (
         Value(..)
       , runExpr
       , equality
       , smallerThen
       , add
       , mul
       , sub
       , modulo
       -- You may include additional exports here, if you want to
       -- write unit tests for them.
       )
       where

import SubsAst

-- You might need the following imports
import Control.Monad
import qualified Data.Map as Map
import Data.Map(Map)


-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)


type Error = String
type Env = Map Ident Value
type Primitive = [Value] -> Either Error Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)

initialContext :: Context
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", equality)
                       , ("<", smallerThen)
                       , ("+", add)
                       , ("*", mul)
                       , ("-", sub)
                       , ("%", modulo)
                       , ("Array", mkArray)
                       ]

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Monad SubsM where
  return x = SubsM $ \(e, _) -> Right (x,e)
  m >>= f = SubsM $ \c@(e, p) -> runSubsM m c >>= \(x, e') -> runSubsM (f x) (e', p)
  fail s = SubsM $ \_ -> Left s

-- You may modify these if you want, but it shouldn't be necessary
instance Functor SubsM where
  fmap = liftM
instance Applicative SubsM where
  pure = return
  (<*>) = ap

equality :: Primitive
equality [IntVal a, IntVal b] = if (a == b) then Right TrueVal else Right FalseVal
equality [UndefinedVal, UndefinedVal] = Right TrueVal
equality [StringVal a, StringVal b] = if (a == b) then Right TrueVal else Right FalseVal
equality [TrueVal, TrueVal] = Right TrueVal
equality [FalseVal, FalseVal] = Right TrueVal
equality [ArrayVal [], ArrayVal []] = Right TrueVal
equality [ArrayVal [], ArrayVal a] = Right FalseVal
equality [ArrayVal a, ArrayVal []] = Right FalseVal
equality [ArrayVal a, ArrayVal b] = if head a == head b then equality [(ArrayVal (tail a)), (ArrayVal (tail b))] else Right FalseVal
equality [_, _] = Right FalseVal
equality _ = Left "Wrong amount of Arguments"

smallerThen :: Primitive
smallerThen [IntVal a, IntVal b] = if (a < b) then Right TrueVal else Right FalseVal
smallerThen [StringVal a, StringVal b] = if (a < b) then Right TrueVal else Right FalseVal
smallerThen [_, _] = Right FalseVal
smallerThen _ = Left "Wrong amount of Arguments"

add :: Primitive
add [IntVal a, IntVal b] = Right (IntVal(a + b))
add [StringVal a, StringVal b] = Right (StringVal(a ++ b))
add [IntVal a, StringVal b] = Right(StringVal(show a ++ b))
add [StringVal a, IntVal b] = Right(StringVal(a ++ show b))
add [_, _] = Left "No Int or String"
add _ = Left "Wrong amount of Arguments"

mul :: Primitive
mul [IntVal a, IntVal b] = Right (IntVal(a*b))
mul [_, _] = Left "No Integer"
mul _ = Left "Wrong amount of Arguments"

sub :: Primitive
sub [IntVal a, IntVal b] = Right (IntVal(a-b))
sub [_, _] = Left "No Integer"
sub _ = Left "Wrong amount of Arguments"

modulo :: Primitive
modulo [IntVal a, IntVal b] = if b == 0 then Left "Division by Zero" else Right (IntVal(mod a b))
modulo [_, _] = Left "No Integer"
modulo _ = Left "Wrong amount of Arguments"

mkArray :: Primitive
mkArray [IntVal n] | n >= 0 = return $ ArrayVal (replicate n UndefinedVal)
mkArray _ = Left "Array() called with wrong number or type of arguments"

modifyEnv :: (Env -> Env) -> SubsM ()
modifyEnv f = SubsM $ \(e, _) -> Right ((),(f e))

putVar :: Ident -> Value -> SubsM ()
putVar name val = modifyEnv $ \e -> Map.insert name val e

getVar :: Ident -> SubsM Value
getVar name = SubsM $ \(e, _) -> case Map.lookup name e of
                                    Just v -> Right (v, e)
                                    Nothing -> Left "No value found in map"

getFunction :: FunName -> SubsM Primitive
getFunction name = SubsM $ \(e, p) -> case Map.lookup name p of
                                        Just v -> Right (v, e)
                                        Nothing -> Left "No value found in map"

evalExpr :: Expr -> SubsM Value
evalExpr Undefined = return UndefinedVal
evalExpr TrueConst = return TrueVal
evalExpr FalseConst = return FalseVal
evalExpr (Number a) = return $ IntVal a
evalExpr (String a) = return $ StringVal a
evalExpr (Array []) = return (ArrayVal [])
evalExpr (Var a) = getVar a

runExpr :: Expr -> Either Error Value
runExpr expr = undefined
