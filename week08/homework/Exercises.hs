module Exercises where

import Test.HUnit

{- Read [instructions.md] first. -}

-- Exercise 1

prog :: IO Int
prog = do
  putStr "Say something: "
  input <- getLine
  let n = length input
  print (n * n)
  return n

prog' :: IO ()
prog' = error "unimplemented"

-- Exercise 2

q1 :: Bool
q1 = error "unimplemented"

q2 :: Bool
q2 = error "unimplemented"

q3 :: Bool
q3 = error "unimplemented"

-- Exercise 3

lengthFile :: FilePath -> IO Int
lengthFile = error "unimplemented"

exercise3a :: Test
exercise3a =
  "lengthFile"
    ~: [ testLength "three/input1.txt" 20,
         testLength "three/input2.txt" 0,
         testLength "three/input3.txt" 11
       ]

concatFiles :: [FilePath] -> FilePath -> IO ()
concatFiles = error "unimplemented"

exercise3b :: Test
exercise3b =
  "concatFiles"
    ~: [ testConcat
           ["three/input1.txt", "three/input2.txt", "three/input3.txt"]
           "three/result.txt"
           "three/expected.txt"
       ]

-- Exercise 4

putStr' :: String -> IO ()
putStr' = error "unimplemented"

putStrLn' :: String -> IO ()
putStrLn' = error "unimplemented"

print' :: Show a => a -> IO ()
print' = error "unimplemented"

---- end of exercises ----

{- Write down the approximate number of hours
it took you to complete this homework. If you have any
comments, feel free to also write them here. -}

time :: Double
time = error "unimplemented"

checkTime :: Test
checkTime = TestCase (assertBool "fill in any time" (time >= 0))

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ exercise3a,
          exercise3b,
          checkTime
        ]
  return ()

-- helper code for testing

testLength :: FilePath -> Int -> Assertion
testLength input n = do
  n' <- lengthFile input
  assertEqual "" n n'

testConcat :: [FilePath] -> FilePath -> FilePath -> Assertion
testConcat inputs output expected = do
  oute <- readFile expected
  concatFiles inputs output
  outr <- readFile output
  assertEqual "" oute outr