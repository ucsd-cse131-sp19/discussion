module Main where

import SimpleJQ.Runner

import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  filename <- case args of
                []  -> do putStrLn "Run with a json file as an argument !"
                          exitFailure
                f:_ -> return f
  result <- run filename
  case result of
    Right json  -> print $ calculate json
    Left errMsg -> do putStrLn errMsg
                      exitFailure
