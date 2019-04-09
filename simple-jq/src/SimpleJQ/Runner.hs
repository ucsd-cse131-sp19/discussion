module SimpleJQ.Runner ( run
                       , calculate
                       ) where

import SimpleJQ.Types
import SimpleJQ.Parser

import qualified Data.Text.IO as TIO

-- | Parses the contents of the given file, and returns the result.
run :: FilePath -> IO (Either String JSON)
run filename = do
  input <- TIO.readFile filename
  return $ parse filename input

calculate :: JSON -> JSON
calculate = id
