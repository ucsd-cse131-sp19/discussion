{-# LANGUAGE OverloadedStrings #-}

module SimpleJQ.Parser ( SimpleJQ.Parser.parse
                       , test
                       ) where

import           SimpleJQ.Types

import           Control.Applicative
import           Data.Text (Text)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

test :: Text -> Maybe JSON
test t = parseMaybe (whole parseJSON) t

-- | Given the filename and the contents of the file, parses it into JSON.
-- Specification is taken from https://www.json.org/
parse :: FilePath -> Text -> Either String JSON
parse fp input =
  case runParser (whole parseJSON) fp input of
    Right j     -> Right j
    Left bundle -> Left $ parseErrorPretty bundle

type Parser = Parsec (ErrorFancy Void) Text

parseJSON :: Parser JSON
parseJSON =
  parseObject     <|>
  parseArray      <|>
  parseJSONString <|>
  parseJSONNumber <|>
  parseBoolean    <|>
  parseNull


parseObject :: Parser JSON
parseObject = between lbrace rbrace (JObject <$> sepBy parseMember comma)
  where
    parseMember = (,) <$> (parseString <* colon) <*> parseJSON

parseJSONString :: Parser JSON
parseJSONString = JString <$> parseString

parseJSONNumber :: Parser JSON
parseJSONNumber = JNumber <$> parseNumber

parseArray :: Parser JSON
parseArray = between lbrack rbrack (JArray <$> sepBy parseJSON comma)

parseBoolean :: Parser JSON
parseBoolean = JBoolean <$> (parseT <|> parseF)
  where
    parseT = symbol "true"  *> return True
    parseF = symbol "false" *> return False

parseNull :: Parser JSON
parseNull = symbol "null" *> return JNull

-- -----------------------------------------------------------------------------
-- Helper Functions
-- -----------------------------------------------------------------------------

parseString :: Parser String
parseString = char '"' *> {- fmap T.pack -} (many validChar) <* char '"'
  where
    validChar :: Parser Char
    validChar = label "character" $ notChar '"'

parseNumber :: Parser Double
parseNumber = do
  n <- parseInt
  f <- parseFrac
  e <- parseExp
  return $ read $ n ++ f ++ e

  where
    parseInt = label "int" $ do
      sign <- optional (char '-')
      rest <- ((:) <$> zero <*> return []) <|>
              ((:) <$> onenine <*> digits)
      return $ case sign of
                 Nothing -> rest
                 Just s  -> s : rest

    zero    = label "zero" $ char '0'
    onenine = label "onenine" $ fromChars "123456789"
    digit   = zero <|> onenine

    digits  = some digit

    fromChars cs = choice [ char c | c <- cs ]
    withDef a p = p <|> return a

    parseFrac = label "frac" $ withDef "" $ (char '.') *> digits

    parseExp = label "exp" $ withDef "" $ do
      e <- fromChars "eE"
      s <- withDef '+' $ fromChars "+-"
      rest <- digits
      return $ e : s : rest
      
whole :: Parser a -> Parser a
whole p = spaceConsumer *> p <* spaceConsumer <* eof

spaceConsumer :: Parser ()
spaceConsumer = L.space (space1 <|> nl) empty empty

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

nl :: Parser ()
nl = newline *> return ()

comma, colon, lbrace, rbrace, lbrack, rbrack :: Parser Char
comma  = char ','
colon  = char ':'
lbrace = char '{'
rbrace = char '}'
lbrack = char '['
rbrack = char ']'
