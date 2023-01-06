module Exercises where

import Test.HUnit (Test (TestList), runTestTT, (~?=))

{-
Exercise 1: Remove the comments that say [ORMOLU_DISABLE] and [ORMOLU_ENABLE].
When you save the file, the extra spaces before the 3 should disappear.
-}
{- ORMOLU_DISABLE -}
format :: Int
format =       3
{- ORMOLU_ENABLE -}

{-
Exercise 2: The following code should have a red underline and an Hlint suggestion
should appear when you hover. Click [Quick Fix], then [Apply hint "Redundant bracket"].
-}
linter :: Int
linter = (3 + 4)

{-
Exercise 3: Replace undefined with 1940.
-}
n :: Int
n = undefined

ntest :: Test
ntest = n ~?= 1940

main :: IO ()
main = do
  print "You have installed Haskell!"
  _ <- runTestTT $ TestList [ntest]
  return ()