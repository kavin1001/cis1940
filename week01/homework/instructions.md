# Homework 1: Haskell Basics

**Due**: Monday, Jan. 23 at 10 p.m.

## Validating Credit Card Numbers

Have you ever wondered how websites validate your credit card
number when you shop online? They don’t check a massive database
of numbers, and they don’t use magic. In fact, most credit providers
rely on a checksum formula called the Luhn algorithm for distinguishing
valid numbers from random collections of digits (or typing mistakes).

In this problem set, you will implement the algorithm, which follows
this specification:

-   Considering the digits of the card number in _reverse order_, double
    the value of every other digit. For example,
    `9455` becomes `[5, 5, 4, 9]` becomes `[5, 10, 4, 18]`.

-   Add the digits of the doubled values and the undoubled digits
    from the original number. For example, `[5, 10, 4, 18]` becomes
    `5 + (1 + 0) + 4 + (1 + 8) = 19`.

-   Calculate the remainder when the sum is divided by 10. For the
    above example, the remainder would be 9.

-   If the result equals 0, then the number is valid.

The progression of Exercises 1–5 shows a common feature of good Haskell programming:
writing smaller functions that each perform a single task and then combining these
pieces to create more complex functions.

Same as Homework 0, you can execute the tests by running

```
> stack ghci Exercises.hs

Prelude> main
```

When all tests pass, the output will be

```
Cases: 23  Tried: 23  Errors: 0  Failures: 0
```

## Grading

This homework will be graded out of 20 points. You will receive three points for each
exercise where you pass all of the autograder tests. (Note that for this assignment,
the tests on Gradescope are the same as the ones provided.)

The final two points will be manually graded for style. You should do your best to
write idiomatic Haskell, such as by effectively using pattern matching, writing type signatures
for every top-level definition, and avoiding unnecessary repetition.

## Source

This assignment is adapted from past offerings of CIS 1940, which in turn adapted it from the first practicum assigned in the University of Utrecht functional programming course taught by Doaitse Swierstra, 2008-2009.
