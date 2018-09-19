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
       , mkArray
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
  m >>= f = SubsM $ \c@(_, p) -> runSubsM m c >>= \(x, e') -> runSubsM (f x) (e', p)
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
equality [ArrayVal [], ArrayVal _] = Right FalseVal
equality [ArrayVal _, ArrayVal []] = Right FalseVal
equality [ArrayVal a, ArrayVal b] = if head a == head b then equality [(ArrayVal (tail a)), (ArrayVal (tail b))] else Right FalseVal
equality [_, _] = Right FalseVal
equality _ = Left "Wrong number of arguments"

smallerThen :: Primitive
smallerThen [IntVal a, IntVal b] = if (a < b) then Right TrueVal else Right FalseVal
smallerThen [StringVal a, StringVal b] = if (a < b) then Right TrueVal else Right FalseVal
smallerThen [_, _] = Right FalseVal
smallerThen _ = Left "Wrong number of arguments"

add :: Primitive
add [IntVal a, IntVal b] = Right (IntVal(a + b))
add [StringVal a, StringVal b] = Right (StringVal(a ++ b))
add [IntVal a, StringVal b] = Right(StringVal(show a ++ b))
add [StringVal a, IntVal b] = Right(StringVal(a ++ show b))
add [_, _] = Left "No Int or String"
add _ = Left "Wrong number of arguments"

mul :: Primitive
mul [IntVal a, IntVal b] = Right (IntVal(a*b))
mul [_, _] = Left "No Integer"
mul _ = Left "Wrong number of arguments"

sub :: Primitive
sub [IntVal a, IntVal b] = Right (IntVal(a-b))
sub [_, _] = Left "No Integer"
sub _ = Left "Wrong number of arguments"

modulo :: Primitive
modulo [IntVal a, IntVal b] = if b == 0 then Left "Division by Zero" else Right (IntVal(mod a b))
modulo [_, _] = Left "No Integer"
modulo _ = Left "Wrong number of arguments"

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
evalExpr (Var a) = getVar a
evalExpr (Array []) = return (ArrayVal [])
evalExpr (Array (a:ax)) = do
  a <- evalExpr a
  ArrayVal ax <- evalExpr(Array ax)
  return (ArrayVal (a:ax))
evalExpr (Compr (ACBody e)) = evalExpr e
evalExpr (Compr (ACFor i e c)) = do
  a <- evalExpr e
  case a of
    ArrayVal xa -> do
      val <- mapM (\x -> do
        putVar i x
        evalExpr(Compr c)) xa
      return (ArrayVal val)
    StringVal xs -> do
      (StringVal s) <- (\x -> evalExpr(Compr c)) (xs)
      return (StringVal s)
    _ -> fail "FOR needs an array or string"

-- (Compr
--   (ACFor "y" (Var "xs")
--     (ACBody (String "a"))))

-- runExpr (Compr (ACFor "y" (Array [Number 0, Number 1, Number 2, Number 3]) (ACBody (String "a"))))
-- Right [ArrayVal [StringVal "a"],[StringVal "a"],[StringVal "a"],[StringVal "a"]]
-- runExpr (Compr (ACFor "y" (String "bcd") (ACBody (String "aggg"))))

evalExpr (Compr (ACIf e c)) = do
  a <- evalExpr e
  case a of
    TrueVal -> evalExpr (Compr c)
    FalseVal -> return (ArrayVal [])
    _ -> fail "IF needs a boolean"

evalExpr (Call a b) = do
  f <- getFunction a
  ArrayVal bv <- evalExpr (Array b)
  case (f bv) of 
    Right r -> return r
    Left l -> fail l

evalExpr (Assign a b) = do
  v <- evalExpr b
  putVar a v
  return v

evalExpr (Comma a b) = do
  _ <- evalExpr a
  evalExpr b

runExpr :: Expr -> Either Error Value
runExpr expr = case (runSubsM (evalExpr expr)) initialContext of
                  Right r -> Right (fst r)
                  Left l -> Left l