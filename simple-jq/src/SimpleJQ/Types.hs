{-# LANGUAGE StrictData #-}

module SimpleJQ.Types ( JSON(..)
                      ) where

import Text.PrettyPrint
import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

-- | JSON data type represents a json value
data JSON =
  -- | a JSON object
  JObject  [(String, JSON)]
  -- | a JSON array
  | JArray   [JSON]
  -- | a JSON string
  | JString  String
  -- | a JSON number
  | JNumber  Double
  -- | true or false
  | JBoolean Bool
  -- | null
  | JNull
  deriving (Eq, Ord)

instance Show JSON where
  show = renderStyle style' . toDoc
    where
      style' = style { lineLength = 80 }

-- -----------------------------------------------------------------------------
-- JSON Pretty Printer
-- -----------------------------------------------------------------------------

class Pretty a where
  toDoc :: a -> Doc

instance Pretty JSON where
  toDoc (JObject kvs) = sep [ lbrace
                            , sep $ punctuate s $ fmap go kvs
                            , rbrace
                            ]
    where
      go (k, j) = nest 2 $ toJSONString k <+> colon <+> toDoc j
      s = comma <> space
  toDoc (JArray js)  = sep [ lbrack
                           , sep $ punctuate comma $ fmap go js
                           , rbrack
                           ]
    where
      go = nest 2 . toDoc
  toDoc (JString s)  = toJSONString s
  toDoc (JBoolean b) = if b then text "true" else text "false"
  toDoc JNull        = text "null"
  toDoc (JNumber n)
    | isInfinite n || isNaN n = text "null"
    | otherwise               = double n

toJSONString :: String -> Doc
toJSONString = doubleQuotes . hcat . fmap oneChar

-- The following functions are taken from
-- http://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html
oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape -> hexEscape c
                      | otherwise  -> char c
  where
    mustEscape = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where
    ch a b = (a, ['\\',b])

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
  where
    d = ord c

smallHex :: Int -> Doc
smallHex x  = text "\\u"
           <> text (replicate (4 - length h) '0')
           <> text h
  where
    h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
  where
    a = (n `shiftR` 10) .&. 0x3ff
    b = n .&. 0x3ff
