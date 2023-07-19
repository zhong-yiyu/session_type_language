import Language.Haskell.TH.Syntax (NameSpace (VarName))
import Text.Parsec
import Text.Parsec.String (Parser)

data Expr
  = Var String
  | Int Int
  | String String
  | Let String Expr Type
  | LetIn String Expr Expr
  | VarType String Type
  | ExprAdd Expr Expr
  deriving (Show)



data Type = IntType | StringType | UnknownType
  deriving (Show)

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
typeParser = choice [string ":Int" >> return IntType, string ":String" >> return StringType, return UnknownType]

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

-- TODO: Need to find a way to separately deal with digits and letters.
-- I can simply treat digits(without any letters) as Ints
-- For now not sure what to do with strings, not even sure strings are actually needed in this language

-- IMPORTANT how many basic types do I need?
-- How about the Session types?


checkExpr :: Expr -> Type -> Bool
checkExpr (ExprAdd e1 e2) IntType = checkExpr e1 IntType && checkExpr e2 IntType
checkExpr num1 IntType = True
checkExpr (VarType _ _) _ = True

-- 在ghci中测试一下内容，应该为True
-- checkExpr (Int 1) IntType