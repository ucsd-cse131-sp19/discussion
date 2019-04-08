{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import SimpleJQ.Runner
import SimpleJQ.Types

import Control.Monad
import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit
import System.Directory
import System.FilePath.Posix

data TestType = Pos | Neg
              deriving (Eq, Show)

main :: IO ()
main = do
  parsingTests <- getParsingTests
  defaultMain $ testGroup "tests" [ parsingTests
                                  , testGroup "other" otherTests
                                  ]

otherTests :: [TestTree]
otherTests = []

getParsingTests :: IO TestTree
getParsingTests = do
  posTests <- getTests "test/parsing-pos" Pos
  negTests <- getTests "test/parsing-neg" Neg
  return $
    testGroup "parsing" [ testGroup "pos" posTests
                        , testGroup "neg" negTests
                        ]

getTests :: FilePath -> TestType -> IO [TestTree] 
getTests dir typ = do
  cwd   <- getCurrentDirectory
  let testsDir = cwd </> dir
  files <- (((testsDir </>) <$>) <$> getDirectoryContents testsDir) >>=
           foldM
           (\fs f -> liftM (\case
                               True  -> f:fs
                               False -> fs) (isJsonFile f)
           )
           []

  return [ testCase (takeFileName filename) $ do
             result <- try (run filename) :: IO (Either SomeException (Either String JSON))
             case typ of
               Pos -> let err msg = assertFailure $ "Error occured while parsing valid JSON file: " ++ msg
                      in case result of
                           Right (Right _)  -> return ()
                           Right (Left msg) -> err msg
                           Left e           -> err (show e)
               Neg -> case result of 
                        Right (Right _) -> assertFailure "Parsing invalid JSON file succeeded."
                        _ -> return ()
         | filename <- files
         ]
  where
    isJsonFile f = (takeExtension f == ".json" &&) <$> doesFileExist f
