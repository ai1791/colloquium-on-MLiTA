import Text.ParserCombinators.Parsec

data Token = Name String | Value String | LP | RP | SO String deriving (Eq, Ord, Show)

parse' :: String -> [Token]
parse' s = case (parse expression "expr" s) of
  Left err -> error (show err)
  Right x -> x
  
lparen =  char '(' >> return LP --сокращение от left parenthesis
rparen =  char ')' >> return RP

oper :: Parser Token
oper = do
  c <- char '+' <|> char '-' <|> char '=' <|> char '*' <|> char '/' <|> char '^' <|> char '.' 
  return $ SO (c:[])

-- value token
value :: Parser Token
value = do
  c <- many1 digit 
  return (Value c)

-- name token
name :: Parser Token
name = do
  a <- many1 letter 
  return (Name a)

-- one token
tok :: Parser Token
tok = do
  a <- try name <|> try value <|> try lparen <|> try rparen <|> try oper 
  skipMany space
  return a

-- all expression
expression :: Parser [Token]
expression = do
  skipMany space
  ts <- many1 tok
  eof
  return ts

main::IO()
main = do
  input <- getLine
  parseTest expression input
