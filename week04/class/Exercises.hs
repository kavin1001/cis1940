module Exercises where

{-
Exercise 1:

Variant of map where if the function returns Nothing,
don't add to result. If returns (Just b), add b to the result.
-}

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe = error "unimplemented"

safeHead :: [a] -> Maybe a
safeHead (x : _) = Just x
safeHead [] = Nothing

{-
For example, if you implemented mapMaybe correctly,

mapMaybe firsts [[], [1, 2], [3], [], [4, 5], []]
should return [1, 3, 4].
-}
firsts :: [[a]] -> [a]
firsts xss = mapMaybe safeHead xss

{-
Exercise 2:

and should take the logical && of a list of bools.
all should return true if all elements of list satisfy the predicate.

Try to write these as concisely as possible.
-}

and :: [Bool] -> Bool
and = error "unimplemented"

all :: (a -> Bool) -> [a] -> Bool
all = error "unimplemented"