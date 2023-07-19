import Text.Parsec
import Text.Parsec.String (Parser)
import Language.Haskell.TH.Syntax (NameSpace(VarName))

data Expr = Var String | Int Int | String String | Let String Expr Type| LetIn String Expr Type Expr
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
  return (Let varName expr varType )

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
  LetIn varName expr varType <$> inExprParser

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