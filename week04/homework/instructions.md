# Homework 3: Recursion Patterns

**Due**: Monday, Feb. 13 at 10 p.m.

## Exercise 1

Implement `safeLast`, which gets the last element of a list (or `Nothing` if the list is empty). **Constraint**: Use `foldr`.

## Exercise 2

Reimplement `concat` and `concatMap` from the list library. `concat` should concatenate the elements in a list of lists. `concatMap` should map a function over the elements of a list and concatenate the results.

What's the shortest solution you can come up with? (Whitespace doesn't count.)

## Exercise 3

Reimplement `func` as `func'` in more idiomatic Haskell style. Use wholemeal programming practices, breaking each function into a pipeline of incremental transformations.

Make sure that your new function still does the same thing! Feel free to write your own tests if you like.

## Grading

This homework will be graded out of 20 points.

Exercise 1 will be worth five autograded points and Exercise 2 will be worth ten autograded points. The conciseness of your Exercise 2 solutions and the wholemeal style of your Exercise 3 solutions will be graded for the remaining five points.
