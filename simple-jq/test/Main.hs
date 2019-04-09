{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import SimpleJQ.Parser
import SimpleJQ.Runner
import SimpleJQ.Types

import qualified Data.Text as T
import Control.Monad
import Control.Exception hiding (assert)
import Test.Tasty
import Test.Tasty.HUnit
import System.Directory
import System.FilePath.Posix

data TestType = Pos | Neg
              deriving (Eq, Show)

main :: IO ()
main = do
  parsingTests <- getParsingTests
  defaultMain $ testGroup "tests" [ testGroup "dummy" dummyTests
                                  , parsingTests
                                  , testGroup "other" otherTests
                                  ]

safe :: Assertion
safe = assert True

dummyTests :: [TestTree]
dummyTests = [ testCase "dummy-1" $ assertEqual "1 /= 1" (1 :: Int) 1
             , testCase "dummy-2" $ assertBool  "1 > 2" ((1 :: Int) < 2)
             , testCase "dummy-3" $ if   True
                                    then safe
                                    else assertFailure "Check failed"
             ]

otherTests :: [TestTree]
otherTests = [ encodeDecodeTest "j1" $
               JObject [ ("foo", JNumber 1)
                       , ("bar", JArray [ JBoolean True
                                        , JBoolean False
                                        , JNull
                                        ])
                       ]
             ]

encodeDecodeTest :: String -> JSON -> TestTree
encodeDecodeTest name j = testCase name $
                      assertEqual "Parse after encoding failed" (Right j) j'
  where
    j'    = parse "" j_str
    j_str = T.pack $ show j

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
