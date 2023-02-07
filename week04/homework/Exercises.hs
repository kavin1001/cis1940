module Exercises where

import Test.HUnit
import Prelude hiding (concat, concatMap)

{- Read [instructions.md] first. -}

-- Exercise 1:

safeLast :: [a] -> Maybe a
safeLast = error "unimplemented"

exercise1 :: Test
exercise1 =
  "safeLast"
    ~: [ safeLast [] ~?= (Nothing :: Maybe ()),
         safeLast [True] ~?= Just True,
         safeLast [1, 2, 3] ~?= Just 3
       ]

-- Exercise 2:

concat' :: [[a]] -> [a]
concat' = error "unimplemented"

exercise2a :: Test
exercise2a =
  "concat"
    ~: [ concat' [] ~?= ([] :: [()]),
         concat' [[1]] ~?= [1],
         concat' [[1, 2], [], [3], [4, 5], []] ~?= [1, 2, 3, 4, 5]
       ]

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' = error "unimplemented"

exercise2b :: Test
exercise2b =
  "concatMap"
    ~: [ concatMap' f [] ~?= [],
         concatMap' f [1] ~?= [1, 1],
         concatMap' f [1, 2, 3] ~?= [1, 1, 2, 2, 3, 3]
       ]
  where
    f :: Int -> [Int]
    f = replicate 2

-- Exercise 3:

func :: [Int] -> Int
func [] = 1
func (x : xs)
  | even x = (x - 2) * func xs
  | otherwise = func xs

func' :: [Int] -> Int
func' = error "unimplemented"

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
        [ exercise1,
          exercise2a,
          exercise2b,
          checkTime
        ]
  return ()