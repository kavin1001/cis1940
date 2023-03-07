{-# LANGUAGE InstanceSigs #-}

module Exercises where

data Foo
  = A Int
  | B Char

instance Eq Foo where
  (==) :: Foo -> Foo -> Bool
  (A i1) == (A i2) = i1 == i2
  (B c1) == (B c2) = c1 == c2
  _ == _ = False

  (/=) :: Foo -> Foo -> Bool
  foo1 /= foo2 = not (foo1 == foo2)

------

data Tree
  = Leaf
  | Branch Tree Int Tree

-- Exercise:
-- Write an Eq instance for Tree.

------

-- number of minutes, number of seconds
data Duration = Duration Int Int

-- Exercise:
-- Write an Eq and Show instance for Duration.
