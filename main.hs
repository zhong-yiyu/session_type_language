import Text.Parsec
  ( ParseError,
    char,
    many1,
    noneOf,
    optional,
    parse,
    sepBy,
  )
import Text.Parsec.String (Parser)
  
parseSignature :: Parser [String]
parseSignature = parseSubsequence `sepBy` char '.'

parseSubsequence :: Parser String
parseSubsequence = many1 (noneOf ".")

parseTypeSignature :: String -> Either ParseError [String]
parseTypeSignature = parse parseSignature ""

outs = parseTypeSignature "DF.SA.CCB.eeC"