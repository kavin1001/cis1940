module Exercises where

import GHC.Float (minusDouble)
import Test.HUnit
  ( Test (..),
    assertBool,
    runTestTT,
    (~:),
    (~?=),
  )
import Prelude hiding (even, max)

{- Read [instructions.md] first. -}

-- Exercise 1:

data Nat
  = Z
  | S Nat
  deriving (Show, Eq)

even :: Nat -> Bool
even Z = True
even (S Z) = False
even (S (S n)) = even n

exercise1a :: Test
exercise1a =
  "even"
    ~: [ even Z ~?= True,
         even (S Z) ~?= False,
         even (S (S Z)) ~?= True,
         even (S (S (S Z))) ~?= False
       ]

max :: Nat -> Nat -> Nat
max Z Z = Z
max (S n) Z = S n
max Z (S n) = S n
max (S a) (S b) = S (max a b)

exercise1b :: Test
exercise1b =
  "max"
    ~: [ max Z Z ~?= Z,
         max (S (S Z)) (S (S (S Z))) ~?= S (S (S Z)),
         max (S (S (S Z))) (S (S Z)) ~?= S (S (S Z))
       ]

-- Exercise 2:

data Arith
  = Num Int
  | Add Arith Arith
  | Mul Arith Arith
  deriving (Show, Eq)

eval :: Arith -> Int
eval (Num a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

exercise2a :: Test
exercise2a =
  "eval"
    ~: [ eval (Num 3) ~?= 3,
         eval (Add (Num 4) (Num 5)) ~?= 9,
         eval (Mul (Add (Num 0) (Num 1)) (Mul (Num 2) (Num 3))) ~?= 6
       ]

opt0 :: Arith -> Arith
opt0 (Num a) = Num a
opt0 (Add (Num 0) a) = opt0 a
opt0 (Add a b) = Add (opt0 a) (opt0 b)
opt0 (Mul a b) = Mul (opt0 a) (opt0 b)

exercise2b :: Test
exercise2b =
  "opt0"
    ~: [ opt0 (Add (Num 3) (Num 2)) ~?= Add (Num 3) (Num 2),
         opt0 (Add (Num 0) (Num 7)) ~?= Num 7,
         opt0 (Mul (Num 2) (Add (Num 0) (Add (Num 0) (Num 1))))
           ~?= Mul (Num 2) (Num 1)
       ]

-- Exercise 3:

data Empty

{- Question:
  Is it possible to write a function of type Empty -> Int?
  If so, write one! If not, briefly explain why. -}

exercise3a :: Empty -> Int
exercise3a _ = 0

{- Question:
  Is it possible to write a function of type Int -> Empty?
  If so, write one! If not, briefly explain why. -}

-- I don't think it is possible to write a function with this type signature because the type
-- Empty has no constructors, so there is no way to create a value of type Empty.

---- end of exercises ----

{- Write down the approximate number of hours
it took you to complete this homework. If you have any
comments, feel free to also write them here. -}

time :: Double
time = 2.0

checkTime :: Test
checkTime = TestCase (assertBool "fill in any time" (time >= 0))

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ exercise1a,
          exercise1b,
          exercise2a,
          exercise2b,
          checkTime
        ]
  return ()
