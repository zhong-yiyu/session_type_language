import Text.ParserCombinators.Parsec (Parser, many1, digit, ParseError, parse, many)
import Text.Parsec.Char (char, satisfy)
import Data.Char
-- parseString :: Parser a -> String -> Either ParseError a
-- parseString p = 

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)

var :: Parser String
var = do
    fc <- firstChar
    rest <- many nonFirstChar
    return (fc:rest)
    where
        firstChar = satisfy (\a -> isLetter a || a == '_')
        nonFirstChar = satisfy (\a -> isLetter a || isDigit a || a == '_')