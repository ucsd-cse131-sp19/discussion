#!/usr/bin/env stack
-- stack --resolver lts-10.0 script

import Control.Monad
import Text.Printf

factorial :: Int -> Int
factorial n =
  if   n <= 1
  then 1
  else n * factorial (n - 1)

factorialT :: Int -> Int
factorialT n = go 1 1
  where
    go acc i = if   i > n
               then acc
               else go (acc * i) (i + 1)

main :: IO ()
main = do
  forM_ [0..20] $ \n ->
    printf "factorial(%2d): %20d\n" n (factorialT n)
