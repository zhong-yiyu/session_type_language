import Control.Exception.Base (assert)
{- AST -}
data Expr
  = EVar String
  | EInt Int
  | EBool Bool
  | EUnit
  | ELetIn String Expr Expr
  | EAnnotate Expr Type
  | EAdd Expr Expr
  -- For session types
  | ESend Expr Expr
  | EReceive Expr
  | EEnd
  deriving (Show)

data Type = TyInt | TyBool | TyUnit
  deriving (Show, Eq)

-- A type environment is actually a collection of variable names and their types.
type TyEnv = [(String, Type)]

{- Typechecker -}

-- Note: At the moment we will fail at runtime if there is a type error.
-- A nicer way of doing this would be to use `Either TypeError (Type, TyEnv)`

-- combine two type environments into one.
combine :: TyEnv -> TyEnv -> TyEnv
combine e1 e2 = e1 ++ e2 -- FIXME: Also need to check that they do not overlap


{-

------------
n => Int ; .


5 => Int ; .
-------------
5 <= Int ; .


x <= Int ; x : Int
---------------------
5 + x => Int; x : Int

-}

-- What can be synthesised?
-- We can synthesise:
--   - Annotated type
--   - Integers, Unit, Bool
--   - Add
--   - Everything else should be a type error!
synth :: Expr -> (Type, TyEnv)
synth (EInt i) = (TyInt, []) -- Synthesise type
synth (EBool i) = (TyBool, [])
synth EUnit = (TyUnit, [])
{-
First try to figure out the type of expr, then check if the synthed type is the same as the annotated type
-}
synth (EAnnotate expr ty) = 
  let synthedType = fst (synth expr) in
  if synthedType == ty then
    (ty, [])
  else
    error $ "Type error: " ++ show expr ++ " has type " ++ show synthedType ++ " but was annotated as " ++ show ty
synth (EAdd e1 e2) =
  let env1 = check e1 TyInt in
  let env2 = check e2 TyInt in
  let env = combine env1 env2 in
  (TyInt, env)
synth bad = error $ "Cannot synthesis type for " ++ show bad


{-
Remember the 'fallback' rule: where we can't check something, but
maybe we can synthesise it?


M => B; env
A = B
--------------
M <= A ; env

-}

-- What can be checked?
--   - Variables
--   - Let
--   - Anything can be synthesised (because of fallback rule)
check :: Expr -> Type -> TyEnv
check (EVar var) ty = [(var, ty)]
{-
First we check if the expr2 has the same type as the "ty" argument, this would genenrate a type environment for the expr2.
Then we find the variable type in expr2, generate a type for varibale
Finally we check if expr1 has type variable's type
If everything is fine, we combine the type environment for expr1 and expr2, after checking those two environments don't overlap
-}
check (ELetIn var expr1 expr2) ty = 
  let in_scope_environment = check expr2 ty in
    case lookup var in_scope_environment 
    of
      Just infered_var_type -> 
        let expr1_environment = check expr1 infered_var_type in
          combine in_scope_environment expr1_environment
      Nothing -> error $ "Type error: " ++ show var ++ " is not in scope"
-- Only invoke fallback if there are no other cases that match.
check other ty =
    let (synthTy, env) = synth other in
    if ty == synthTy then
        env
    else
        error $ "Type error: fallback rule for " ++ show other

-- let testExpr1 = ELetIn "x" (EInt 5) (EAdd (EVar "x") (EInt 5))
testExprAnnotate = EAnnotate (EVar "x") TyInt
-- This Var expr alone should report an error in the "synth" function, because there's nothing to synthesize from.
testExprVar = EVar "x"

-- This expression should pass the synth function and get a type environment from the check function.
testExprAdd = EAdd (EInt 5) (EInt 5)

-- Here's where we define the session types, there're three types of session types, send, receive and end.
data SessionType
  = Send Type SessionType
  | Receive Type SessionType
  | End
  deriving (Show, Eq)

-- So an example Session type would be
exampleSessionType = Send TyInt (Receive TyInt End) -- Not sure if the "End" is necessary in the end.

-- This function is about duality, it takes a session type and returns its dual.
dualitySessionType :: SessionType -> SessionType
dualitySessionType (Send ty sessionType) = Receive ty (dualitySessionType sessionType)
dualitySessionType (Receive ty sessionType) = Send ty (dualitySessionType sessionType)
dualitySessionType End = End
-- To test its correctness, we can do 
testDuality = dualitySessionType (Send TyInt (Receive TyInt End)) == Receive TyInt (Send TyInt End) -- It should return True

-- The function "let func(channel) = send (12, channel) in End" should have the type "Send (Int, End)", and the AST of the function is
-- ELetIn "func" (ESend (EInt 12) (EVar "channel")) EEnd