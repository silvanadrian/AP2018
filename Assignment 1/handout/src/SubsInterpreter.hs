module SubsInterpreter
       (
         Value(..)
       , runExpr
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
                       , ("+", undefined)
                       , ("*", undefined)
                       , ("-", undefined)
                       , ("%", undefined)
                       , ("Array", mkArray)
                       ]

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Monad SubsM where
  return x = undefined 
  m >>= f = undefined
  fail s = undefined

-- You may modify these if you want, but it shouldn't be necessary
instance Functor SubsM where
  fmap = liftM
instance Applicative SubsM where
  pure = return
  (<*>) = ap

equality:: Primitive
equality a = if length a > 2 then equality2 (head a) (head (tail a)) else Left "List is smaller or bigger then 2"

equality2:: Value -> Value -> Either Error Value
equality2 (IntVal a) (IntVal b) = if (a == b) then Right TrueVal else Right FalseVal
equality2 UndefinedVal UndefinedVal = Right TrueVal
equality2 (StringVal a) (StringVal b) = if (a == b) then Right TrueVal else Right FalseVal
equality2 TrueVal TrueVal = Right TrueVal
equality2 FalseVal FalseVal = Right TrueVal
equality2 (ArrayVal []) (ArrayVal []) = Right TrueVal
equality2 (ArrayVal a) (ArrayVal b) = if head a == head b then equality2 (ArrayVal (tail a)) (ArrayVal (tail b)) else Right FalseVal
equality2 _ _ = Right FalseVal

smallerThen:: Primitive
smallerThen a = if length a > 2 then smallerThen2 (head a) (head (tail a)) else Left "List is smaller or bigger then 2"

smallerThen2:: Value -> Value -> Either Error Value
smallerThen2 (IntVal a) (IntVal b) = if (a < b) then Right TrueVal else Right FalseVal
smallerThen2 (StringVal a) (StringVal b) = if (a < b) then Right TrueVal else Right FalseVal

mkArray :: Primitive
mkArray [IntVal n] | n >= 0 = return $ ArrayVal (replicate n UndefinedVal)
mkArray _ = Left "Array() called with wrong number or type of arguments"

modifyEnv :: (Env -> Env) -> SubsM ()
modifyEnv f = undefined

putVar :: Ident -> Value -> SubsM ()
putVar name val = undefined

getVar :: Ident -> SubsM Value
getVar name = undefined

getFunction :: FunName -> SubsM Primitive
getFunction name = undefined

evalExpr :: Expr -> SubsM Value
evalExpr expr = undefined

runExpr :: Expr -> Either Error Value
runExpr expr = undefined
