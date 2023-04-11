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

prog' :: IO Int
prog' =
  putStr "Say something: "
    >> getLine
    >>= \input ->
      let n = length input
       in print (n * n)
            >> return n

-- Exercise 2

q1 :: Bool
q1 = True

q2 :: Bool
q2 = False

q3 :: Bool
q3 = True

-- Exercise 3

lengthFile :: FilePath -> IO Int
lengthFile x = do
  contents <- readFile x
  return (length contents)

exercise3a :: Test
exercise3a =
  "lengthFile"
    ~: [ testLength "three/input1.txt" 20,
         testLength "three/input2.txt" 0,
         testLength "three/input3.txt" 11
       ]

concatFiles :: [FilePath] -> FilePath -> IO ()
-- use mapM
concatFiles inputFiles outputFile = do
  fileContents <- mapM readFile inputFiles
  writeFile outputFile (concat fileContents)

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
putStr' [] = return ()
putStr' (x : xs) = do
  putChar x
  putStr' xs

putStrLn' :: String -> IO ()
putStrLn' x = do
  putStr' x
  putChar '\n'

print' :: Show a => a -> IO ()
print' x = putStrLn' (show x)

---- end of exercises ----

{- Write down the approximate number of hours
it took you to complete this homework. If you have any
comments, feel free to also write them here. -}

time :: Double
time = 2.5

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