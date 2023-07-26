import Language.Haskell.TH.Syntax (NameSpace (VarName), Con)
import Text.Parsec
import Text.Parsec.String (Parser)

data Expr
  = Var String
  | Int Int
  | Let String Expr Type
  | LetIn String Expr Expr
  | VarType String Type
  | ExprAdd Expr Expr
  deriving (Show)

newtype TypeEnv = TypeEnv [(String, Type)]
  deriving (Show)



data Type = IntType | UnknownType | UnitType| LetType
  deriving (Show, Eq)

letExpression :: Parser Expr
letExpression = do
  string "let"
  spaces
  varName <- many1 letter
  spaces
  varType <- typeParser
  spaces
  char '='
  spaces
  expr <- expression
  return (Let varName expr varType)

letInExpression :: Parser Expr
letInExpression = do
  string "let"
  spaces
  varName <- many1 letter
  spaces
  varType <- typeParser
  spaces
  char '='
  spaces
  expr <- expression
  let inExpr = VarType varName varType
  return (LetIn varName inExpr expr)

-- By detecting the ":" character, we can determine if the type has an annotation
typeParser :: Parser Type
typeParser = choice [string ":Int" >> return IntType, return UnknownType]

inExprParser :: Parser Expr
inExprParser = do
  spaces
  string "in"
  spaces
  expression

-- TODO: Find some way to deal with the "in" statement's scope.
-- What exactly is the scope in this language?

expression :: Parser Expr
expression = do
  varName <- many1 letter <|> many1 digit
  return (Var varName)

parseLetStatement :: String -> Either ParseError Expr
parseLetStatement = parse letExpression ""

-- TODO: Implement the checking function.
checkLetStatement :: Expr -> Type -> Bool
checkLetStatement = undefined

checkExprWithEnv :: Expr -> Type -> TypeEnv
checkExprWithEnv (Var varName) varType = TypeEnv [(varName, varType)]

-- This function is the "default" or "catch-all" case, normally used as last resort
checkExprWithEnv e ty = 
  let (synthTy, env) = synthExpr e in
    if synthTy == ty then env else error "Type mismatch"


combineTypeEnv :: TypeEnv -> TypeEnv -> TypeEnv
combineTypeEnv (TypeEnv env1) (TypeEnv env2) = TypeEnv (env1 ++ env2)

synthExpr :: Expr -> (Type, TypeEnv)
synthExpr (Let {}) = (LetType, TypeEnv [])
synthExpr (ExprAdd e1 e2) = 
  let env1 = checkExprWithEnv e1 IntType in
    let env2 = checkExprWithEnv e2 IntType in
      (IntType, combineTypeEnv env1 env2)
synthExpr (VarType varName varType) = (varType, TypeEnv [(varName, varType)])
synthExpr (Int _) = (IntType, TypeEnv [])
synthExpr _ = error "Not implemented"