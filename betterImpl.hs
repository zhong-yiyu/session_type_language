{- AST -}
data Expr
  = EVar String
  | EInt Int
  | EBool Bool
  | EUnit
  | ELetIn String Expr Expr
  | EAnnotate Expr Type
  | EAdd Expr Expr
  deriving (Show)

data Type = TyInt | TyBool | TyUnit
  deriving (Show, Eq)

type TyEnv = [(String, Type)]

{- Typechecker -}

-- Note: At the moment we will fail at runtime if there is a type error.
-- A nicer way of doing this would be to use `Either TypeError (Type, TyEnv)`

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
synth (EBool i) = undefined
synth EUnit = undefined
synth (EAnnotate ty) = undefined
synth (EAdd e1 e2) =
  let env1 = check e1 TyInt in
  let env2 = check e2 TyInt in
  let env = combine env1 env2 in
  (TyInt, env)
synth bad = error $ "Cannot synthesis type for " ++ (show bad)


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
check (ELet var e1 e2) ty = undefined
-- Only invoke fallback if there are no other cases that match.
check other ty =
    let (synthTy, env) = synth other in
    if ty == synthTy then
        env
    else
        error $ "Type error: fallback rule for " ++ (show other)

