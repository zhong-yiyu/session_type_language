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
  -- The subsequence is a sequence of characters that are not "." symbol.
  -- And the extracted subsequence is a list of characters.
  -- for those characters, we detect if there's a "?" or "!" symbol.
  -- If there is, we return the symbol, otherwise we return the whole subsequence.

parseTypeSignature :: String -> Either ParseError [String]
parseTypeSignature = parse parseSignature ""




keywordSend :: Parser String
-- The keyword is just "!" symbol, the symbol is followed by and only by "." symbol.
keywordSend = do
  char '!'
  optional (char '.')
  return "!"

keywordReceive :: Parser String
-- The keyword is just "?" symbol, the symbol is followed by and only by "." symbol.
keywordReceive = do
  char '?'
  optional (char '.')
  return "?"



outs = parseTypeSignature "DF.SA.CCB.eeC"