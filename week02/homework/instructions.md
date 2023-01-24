# Homework 2: Algebraic Data Types

**Due**: Monday, Jan. 30 at 10 p.m.

Please note that the three exercises are disjoint.

## Exercise 1

One way to represent natural numbers is using a data type with two constructors:

```Haskell
data Nat
  = Z
  | S Nat
```

The `Z` constructor represents the number `0`. The `S` constructor stands for "successor,"
and `S n` represents the number that follows `n`: that is, `n + 1`. For example, we would
write `1` as `S Z` and `2` as `S (S Z)`.

**Constraint**: You may _not_ solve any exercise with `Nat`s by convering to and from `Int`s.

Fill in these two functions:

-   `even n` should return `True` if `n` represents an even number and `False` otherwise.

-   `max n m` should return the larger of `n` or `m`.

## Exercise 2

We define a data type for arithmetic expressions, which supports addition
(with the `Add` constructor) and multiplication (with the `Mul` constructor).

```Haskell
data Arith
  = Num Int
  | Add Arith Arith
  | Mul Arith Arith
```

Given an arbitrarily complex `Arith` expression, we want to evaluate it to an `Int`.
For example,

-   `Num 3` should evaluate to `3`.
-   `Add (Num 4) (Num 5)` should evaluate to `4 + 5 = 9`.
-   `Mul (Add (Num 0) (Num 1)) (Mul (Num 2) (Num 3))` should evaluate to `(0 + 1) * (2 * 3) = 6`.

Fill in the `eval` function.

We can simplify expressions by changing every occurence of `Add (Num 0) e` to just `e`,
since `0 + n = n`. (Of course, `n + 0 = n` too, but for simplicity, we only consider the case on the left.)

Fill in the `opt0` function so that on inputs such as

```Haskell
Mul (Num 2)
    (Add (Num 0)
         (Add (Num 0) (Num 1)))
```

it will output

```Haskell
Mul (Num 2) (Num 1)
```

## Exercise 3

Let's do a (hopefully) fun thought experiment. Consider a data type with no constructors.

```Haskell
data Empty
```

As suggested by the name, there is no way to construct any values of type `Empty`.
Answer the two questions in the exercise file.

## Grading

This homework will be graded out of 20 points.
Exercise 1 and 2 will be autograded and worth four points per subproblem.
Exercise 3 will be manually graded and worth two points.

The final two points will be manually graded for style. We will pay attention in particular
to your use of pattern matching.

## Source

Aspects of this assignment take inspiration from _Software Foundations_.
