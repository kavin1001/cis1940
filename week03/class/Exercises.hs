module Exercises where

-- Exercise 1:

-- Store a person's name and age.
data Person = Person String Int
  deriving (Show)

{- Get the names of the people in a list
   who are at most 18 years old.
-}
youngNames :: [Person] -> [String]
youngNames = error "unimplemented"

{- As usual, in a terminal in this folder, run
   $ stack ghci Exercises.hs

   Then, at the prompt, you can evaluate your code!
   For example,
   > youngNames peopleInput

   should return ["Bob", "Jill"]
-}

peopleInput :: [Person]
peopleInput = [Person "Bob" 12, Person "Jack" 23, Person "Jill" 18, Person "Alice" 70]

-- Exercise 2:

-- Reimplement map and filter using foldr.

map' :: (a -> b) -> [a] -> [b]
map' = error "unimplemented"

filter' :: (a -> Bool) -> [a] -> [a]
filter' = error "unimplemented"