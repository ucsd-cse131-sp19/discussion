{-# LANGUAGE StrictData #-}

module SimpleJQ.Types ( JSON(..)
                      ) where

import Data.Text
import Text.PrettyPrint

data JSON = JObject  [(String, JSON)]
          | JArray   [JSON]
          | JString  String
          | JNumber  Double
          | JBoolean Bool
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

instance Pretty Text where
  toDoc = text . unpack

instance Pretty JSON where
  toDoc (JObject kvs) = sep [ lbrace
                            , sep $ punctuate s $ fmap go kvs
                            , rbrace
                            ]
    where
      go (k, j) = nest 2 $ doubleQuotes (text k) <+> colon <+> toDoc j
      s = comma <> space
  toDoc (JArray js)  = sep [ lbrack
                           , sep $ punctuate comma $ fmap go js
                           , rbrack
                           ]
    where
      go = nest 2 . toDoc
  toDoc (JString s)  = doubleQuotes $ text s
  toDoc (JBoolean b) = if b then text "true" else text "false"
  toDoc JNull        = text "null"
  toDoc (JNumber n)
    | isInfinite n || isNaN n = text "null"
    | otherwise               = double n
