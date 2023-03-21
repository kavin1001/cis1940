# Homework 7: Monads

**Due**: Monday, March 27 at 10 p.m.

## Exercise 1 (7 points)

Write a function that detects whether or not a string is "good."
A string is "good" if

1. The string is empty, or
2. It starts with a digit `n`.
3. The digit is then followed by `n` of the letter `a`.
4. The rest of the string also follows this pattern.

For example, `3aaa2aa` is good, while `3aaa2a` is not.

Fill in the provided function skeleton. You should make effective use of the
`Maybe` monad and `do`-notation. In particular, you may **not** explicitly use
`Just` or `Nothing` anywhere in your solution.

Hint: You will want to use a few library functions. These include
`readMaybe :: Read a => String -> Maybe a` from `Text.Read` and
`stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]` from `Data.List`.

## Exercise 2 (4 points)

Write a function that takes an integer `n` and returns all of the pairs of
integers from 0 to `n` (recall the notation `[0 .. n]`) that sum up to `n`.

Implement this function twice: `sumsTo` should use `do`-notation, and `sumsTo'`
should use list comprehension.

## Exercise 3 (9 points)

(Re)-implement the functions with the provided type signatures. I am
deliberately not explaining in words what the functions do — instead, let the
types guide you!

You may **not** use `fmap` or `<$>` in your solutions.
(Can you figure out why this is a restriction?)

## Grading

This homework will be graded out of 20 points. Exercise 3 will be graded
manually. The other two exercises will be mostly autograded, with some
style points. See Gradescope for the full breakdown.
