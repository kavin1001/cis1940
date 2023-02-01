# Homework 3: Recursion Patterns

**Due**: Monday, Feb. 6 at 10 p.m.

**Constraint**: You may _not_ write any recursive functions in this homework.

## Exercise 1

Reimplement the functions `or` and `any` in accordance with the constraints below. (We add primes `'` to avoid name conflicts with existing functions.)

a. The `or'` function returns `True` if one or more of the elements in the list is `True` and `False` otherwise. **Constraint**: Use `foldr`.

b. For the `any` function, `any f xs` returns `True` when there is an element `x` in `xs` is such that `f x = True`. **Constraint**: Implement this twice. For `any'`, use `map` and `or'`. For `any''`, use `foldr`.

## Exercise 2

Implement this (admittedly very silly!) function. On an input such as

```Haskell
"I have loads of fun words"
```

the function should output

```Haskell
["I", "o,f", "f,u,n", "w,o,r,d,s"]
```

That is, the function separates the string into words, discards the words containing the letter `a`, and for each word, adds a comma between each character.

As part of this exercise, you will want to find useful helper functions from Haskell's list library.

The documentation for `Data.List` is [here](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-List.html), but you are encouraged to make extensive use of [Hoogle](https://hoogle.haskell.org/?scope=package%3Abase). An important Haskell skill is to figure out the _type_ of the thing you want and to use that to find its name!

## Grading

This homework will be graded out of 20 points.

Exercise 1 will be worth six autograded points and Exercise 2 will be worth nine autograded points. The final five points will be manually graded to check that you followed the constraints and for style.
