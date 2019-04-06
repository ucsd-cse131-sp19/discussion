module Main where

import SimpleJQ.Calculate
import SimpleJQ.Parser

import           System.Environment
import           System.Exit
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  filename <- case args of
                []  -> putStrLn "Run with a json file as an argument !" >> exitFailure
                f:_ -> return f
  input <- TIO.readFile filename
  case parse filename input of
    Right json  -> print $ calculate json
    Left errMsg -> do putStrLn errMsg
                      exitFailure
