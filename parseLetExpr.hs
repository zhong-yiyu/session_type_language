import Text.Parsec
import Text.Parsec.String (Parser)
import Language.Haskell.TH.Syntax (NameSpace(VarName))

data LetStmt = Let String Expr  Type
    deriving (Show)

data Expr = Var String | Int Int | String String
    deriving (Show)

data Type = IntType | StringType | UnknownType
    deriving (Show)

letExpression :: Parser LetStmt
letExpression = do
  string "let"
  spaces
  varName <- many1 letter
  spaces
--   If there's no ":" symbol, we assume the type is UnknownType
  varType <- typeParser
--   varType = option UnknownType (try (do 
--     char ':'
--     spaces
--     typeParser
--     ))
  spaces
  char '='
  spaces
  expr <- expression
  return (Let varName expr varType)

-- 通过解析是否有":"符号判断是否有类型，如果没有符号，就返回UnknownType
typeParser :: Parser Type
typeParser = choice [string ":Int" >> return IntType, string ":String" >> return StringType, return UnknownType]


expression :: Parser Expr
expression = do
  varName <- many1 letter <|> many1 digit
  return (Var varName)

parseLetStatement :: String -> Either ParseError LetStmt
parseLetStatement = parse letExpression ""