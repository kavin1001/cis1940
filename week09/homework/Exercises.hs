module Exercises where

import Control.Monad (guard)
import Data.List (sort, stripPrefix)
import Data.Maybe (isJust)
import Test.HUnit
import Text.Read (readMaybe)

{- Read [instructions.md] first. -}

-- Exercise 1

isGood :: String -> Bool
isGood "" = True
isGood s = case readMaybe [head s] of
  Just n -> do
    let prefix = replicate n 'a'
    maybe False isGood (stripPrefix prefix (tail s))
  Nothing -> False

exercise1 :: Test
exercise1 =
  "isGood"
    ~: [ all isGood ["", "3aaa2aa", "9aaaaaaaaa", "0", "001a", "2aa2aa"] ~?= True,
         not (any isGood ["3aaa2a", "4aaa", "1", "100a", "2bb2bb"]) ~?= True
       ]

-- Exercise 2

sumsTo :: Int -> [(Int, Int)]
sumsTo x = do
  a <- [0 .. x]
  let b = x - a
  guard (b >= 0)
  return (a, b)

sumsTo' :: Int -> [(Int, Int)]
sumsTo' n = [(x, y) | x <- [0 .. n], y <- [0 .. n], x + y == n]

exercise2 :: Test
exercise2 =
  "sumsTo"
    ~: [ sameElems (sumsTo 3) threes ~?= True,
         sameElems (sumsTo' 3) threes ~?= True
       ]
  where
    threes = [(0, 3), (1, 2), (2, 1), (3, 0)]

-- Exercise 3

join :: Monad m => m (m a) -> m a
join mma = do
  ma <- mma
  ma

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = do
  a <- ma
  return (f a)

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = \a -> do
  b <- f a
  g b

---- end of exercises ----

{- Write down the approximate number of hours
it took you to complete this homework. If you have any
comments, feel free to also write them here. -}

time :: Double
time = 3.5

checkTime :: Test
checkTime = TestCase (assertBool "fill in any time" (time >= 0))

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ exercise1,
          exercise2,
          checkTime
        ]
  return ()

-- helper functions for tests

sameElems :: Ord a => [a] -> [a] -> Bool
sameElems xs ys = sort xs == sort ys