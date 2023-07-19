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
  inexpr <- inExprParser
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

-- 通过解析是否有":"符号判断是否有类型，如果没有符号，就返回UnknownType
typeParser :: Parser Type
typeParser = choice [string ":Int" >> return IntType, string ":String" >> return StringType, return UnknownType]

inExprParser :: Parser Expr
inExprParser = do
    spaces
    string "in"
    spaces
    expression

expression :: Parser Expr
expression = do
    varName <- many1 letter <|> many1 digit
    return (Var varName)

parseLetStatement :: String -> Either ParseError Expr
parseLetStatement = parse letExpression ""