{-# LANGUAGE InstanceSigs #-}

module Exercises where

import Data.List
import Test.HUnit

{- Read [instructions.md] first. -}

newtype Poly = P [Int]

-- Exercise 1

instance Eq Poly where
  (==) :: Poly -> Poly -> Bool
  (==) = error "unimplemented"

exercise1 :: Test
exercise1 =
  "eq"
    ~: [ P [1, 2, 3] == P [1, 2, 3] ~?= True,
         P [1, 2] == P [1, 2, 3] ~?= False,
         P [1, 2, 0] == P [1, 2] ~?= True,
         P [] == P [0, 0] ~?= True,
         P [1, 0, 2] == P [1, 2] ~?= False
       ]

-- Exercise 2

instance Show Poly where
  show :: Poly -> String
  show = error "unimplemented"

exercise2 :: Test
exercise2 =
  "show"
    ~: [ show (P [1, 2, 3]) ~?= "1 + 2x + 3x^2",
         show (P [-1, 0]) ~?= "-1 + 0x",
         show (P []) ~?= "0"
       ]

-- Exercise 3

instance Num Poly where
  -- These will be implemented later.

  (+) :: Poly -> Poly -> Poly
  (+) = plus

  (*) :: Poly -> Poly -> Poly
  (*) = times

  -- Implement these now.

  negate :: Poly -> Poly
  negate = error "unimplemented"

  fromInteger :: Integer -> Poly
  fromInteger = error "unimplemented"

  -- Leave these unimplemented;
  -- no meaningful definition exists.

  abs :: Poly -> Poly
  abs = undefined

  signum :: Poly -> Poly
  signum = undefined

exercise3a :: Test
exercise3a =
  "negate"
    ~: [ negate (P [1, 2, 3]) ~?= P [-1, -2, -3],
         negate (P [-1, 0]) ~?= P [1, 0],
         negate (P []) ~?= P []
       ]

exercise3b :: Test
exercise3b =
  "fromInteger"
    ~: [ 3 ~?= P [3],
         0 ~?= P [0]
       ]

-- Exercise 4

plus :: Poly -> Poly -> Poly
plus = error "unimplemented"

exercise4 :: Test
exercise4 =
  "sum"
    ~: [ P [0, 1, 2] + P [1, 0, 2] ~?= P [1, 1, 4],
         P [5, 1] + P [1, 1, 3] ~?= P [6, 2, 3],
         P [1, 1, 3] + P [5, 1] ~?= P [6, 2, 3],
         P [] + P [] ~?= P []
       ]

-- Exercise 5

times :: Poly -> Poly -> Poly
times = error "unimplemented"

exercise5 :: Test
exercise5 =
  "times"
    ~: [ P [1, 1, 1] * P [2, 2] ~?= P [2, 4, 4, 2],
         P [2, 2] * P [1, 1, 1] ~?= P [2, 4, 4, 2],
         P [1, 2, 3] * P [4, 5, 6] ~?= P [4, 13, 28, 27, 18],
         P [] * P [1, 2, 3] ~?= P []
       ]

-- Tying it all together:

x :: Poly
x = P [0, 1]

final :: Test
final = (2 * x - 1) * (x + 2) == (2 * x ^ 2 + 3 * x - 2) ~?= True

---- end of exercises ----

{- Write down the approximate number of hours
it took you to complete this homework. If you have any
comments, feel free to also write them here. -}

time :: Double
time = error "unimplemented"

checkTime :: Test
checkTime = TestCase (assertBool "fill in any time" (time >= 0))

{- Please indicate which exercises you used a
hint on. For example, if you used a hint on Exercises
1 and 3, record [True, False, True, False, False].

There is no bearing on your grade — this is just
useful data for me to have. It's perfectly fine to
have used hints on most or all of the problems.
-}

hints :: [Bool]
hints = error "unimplemented" -- [False, False, False, False, False]

checkHints :: Test
checkHints = TestCase (assertBool "fill in hint info" (length hints == 5))

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ exercise1,
          exercise2,
          exercise3a,
          exercise3b,
          exercise4,
          exercise5,
          final,
          checkTime,
          checkHints
        ]
  return ()