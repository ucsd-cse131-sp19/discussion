{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module SimpleJQ.Parser ( SimpleJQ.Parser.parse
                       , parse'
                       ) where

import           SimpleJQ.Types

import           Control.Applicative
import           Data.Char
import           Data.Text (Text, pack)
import           Data.Void
import           Text.Printf
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Given the filename and the contents of the file, parses it into JSON.
-- Specification is taken from https://www.json.org/
parse :: FilePath -> Text -> Either String JSON
parse fp input =
  case runParser (whole parseJSON) fp input of
    Right j     -> Right j
    Left bundle -> Left $ parseErrorPretty bundle

-- | Parse the given string as a json value.
parse' :: String -> Maybe JSON
parse' = parseMaybe (whole parseJSON) . pack

type Parser = Parsec (ErrorFancy Void) Text

parseJSON :: Parser JSON
parseJSON = withSC $
  parseObject     <|>
  parseArray      <|>
  parseJSONString <|>
  parseJSONNumber <|>
  parseBoolean    <|>
  parseNull


parseObject :: Parser JSON
parseObject = between lbrace rbrace (withSC $ JObject <$> sepBy parseMember (withSC comma))
  where
    parseMember = (,) <$> (parseString <* withSC colon) <*> parseJSON

parseJSONString :: Parser JSON
parseJSONString = withSC $ JString <$> parseString

parseJSONNumber :: Parser JSON
parseJSONNumber = withSC $ JNumber <$> parseNumber

parseArray :: Parser JSON
parseArray = between lbrack rbrack (withSC $ JArray <$> sepBy parseJSON (withSC comma))

parseBoolean :: Parser JSON
parseBoolean = withSC $ JBoolean <$> (parseT <|> parseF)
  where
    parseT = symbol "true"  *> return True
    parseF = symbol "false" *> return False

parseNull :: Parser JSON
parseNull = withSC $ symbol "null" *> return JNull

-- -----------------------------------------------------------------------------
-- Helper Functions
-- -----------------------------------------------------------------------------

parseString :: Parser String
parseString = char '"' *> many validChar <* char '"'
  where
    getc :: Parser Char
    getc = notChar '"'

    validChar :: Parser Char
    validChar = label "character" $ do
      c1 <- getc
      if | generalCategory c1 == Control ->
           customFailure $ ErrorFail $ "got control character inside string"
         | c1 == '\\' -> do
             c2 <- printChar
             case c2 of
               '"'  -> return '"'
               '\\' -> return '\\'
               '/'  -> return '/'
               'b'  -> return '\b'
               'f'  -> return '\f'
               'n'  -> return '\n'
               'r'  -> return '\r'
               't'  -> return '\t'
               'u'  -> do digits <- count 4 hexDigitChar
                          let n = read ("0x" ++ digits) :: Int
                          return $ toEnum n
               _ -> customFailure $ ErrorFail $ printf "got unrecognized character %c after \\" c2
         | otherwise -> return c1

parseNumber :: Parser Double
parseNumber = do
  n <- parseInt
  f <- parseFrac
  e <- parseExp
  let str = n ++ f ++ e
  return $ read str

  where
    parseInt = label "int" $ do
      sign <- optional (char '-')
      rest <- ((:) <$> zero <*> return []) <|>
              ((:) <$> onenine <*> many digit)
      return $ case sign of
                 Nothing -> rest
                 Just s  -> s : rest

    zero    = label "zero" $ char '0'
    onenine = label "onenine" $ fromChars "123456789"
    digit   = zero <|> onenine

    fromChars cs = choice [ char c | c <- cs ]
    withDef a p = p <|> return a

    parseFrac = label "frac" $ withDef "" $ (:) <$> (char '.') <*> some digit

    parseExp = label "exp" $ withDef "" $ do
      e <- fromChars "eE"
      s <- withDef '+' $ fromChars "+-"
      rest <- some digit
      return $ e : s : rest
      
whole :: Parser a -> Parser a
whole p = withSC p <* eof

withSC :: Parser a -> Parser a
withSC p = spaceConsumer *> p <* spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = L.space spc empty empty
  where
    spc = choice (char <$> ws) >> return ()
    ws = [ toEnum n :: Char
         | n <- [9, 0xA, 0xD, 0x20]
         ]

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

comma, colon, lbrace, rbrace, lbrack, rbrack :: Parser Char
comma  = char ','
colon  = char ':'
lbrace = char '{'
rbrace = char '}'
lbrack = char '['
rbrack = char ']'
