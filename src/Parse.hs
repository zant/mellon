module Parse (readExpr, KellogsVal (..)) where

import Text.ParserCombinators.Parsec hiding (spaces)

data KellogsVal
  = Atom String
  | List [KellogsVal]
  | DottedList [KellogsVal] KellogsVal
  | Number Integer
  | String String
  | Bool Bool
  deriving (Show)

parseString :: Parser KellogsVal
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ String x

parseAtom :: Parser KellogsVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser KellogsVal
parseNumber = Number . read <$> many1 digit

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseList :: Parser KellogsVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser KellogsVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

parseQuoted :: Parser KellogsVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseLists :: Parser KellogsVal
parseLists = do
  _ <- char '('
  x <- try parseList <|> parseDottedList
  _ <- char ')'
  return x

parseExpr :: Parser KellogsVal
parseExpr = parseAtom <|> parseString <|> parseNumber <|> parseQuoted <|> parseLists

readExpr :: String -> KellogsVal
readExpr input = case (parse parseExpr) "Kellogs" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val