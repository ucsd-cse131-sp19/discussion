module SimpleJQ.Runner (run) where

import SimpleJQ.Types
import SimpleJQ.Parser

import qualified Data.Text.IO as TIO

run :: FilePath -> IO (Either String JSON)
run filename = do
  input <- TIO.readFile filename
  return $ parse filename input
