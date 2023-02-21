# Hints

Please do not read these until you have spent some time thinking about the problem!

## Exercise 1

Consider this partial solution:

```Haskell
instance Eq Poly where
  (==) :: Poly -> Poly -> Bool
  P [] == P cs = _
  P cs == P [] = _
  P (c1 : c1s) == P (c2 : c2s) = _
```

For the first two cases, observe that `P []` represents the zero polynomial.
What needs to be true about `cs` for `P cs` to also represent the zero
polynomial? Try to find a list library function to help you with this.

For the last case, to check the rest of the polynomial, you should recurse.

## Exercise 2

Consider this partial solution:

```Haskell
instance Show Poly where
  show :: Poly -> String
  show (P []) = "0"
  show (P cs) = _ (zipWith showTerm cs [0 ..])
    where
      showTerm :: Int -> Int -> String
      showTerm = _
```

We break this problem down into several steps. First, define the helper function
`showTerm`, which given a coefficient (the first agument) and an exponent (the
second argument), generates the string for that term.

Then, we use `zipWith`, which allows us to combine the list of coefficients and
the list of exponents with `showTerm`. Notice that `[0 ..]` represents the
infinite list of numbers starting from zero. Why is this okay?

Finally, combine the strings for each term using a list library function.

## Exercise 3

These should be simple one-liners!

## Exercise 4

Consider this partial solution:

```Haskell
plus (P c1s) (P c2s) = P (zipWith (+) c1s c2s)
```

This does not quite work because `zipWith` ignores the extra elements if the
input lists are not the same length. There are many approaches to fixing this
problem; one is to pad the shorter list with zeroes.

## Exercise 5

Consider this partial solution:

```Haskell
times :: Poly -> Poly -> Poly
times (P c1) (P c2) = sum (go c1 c2)
  where
    go :: [Int] -> [Int] -> [Poly]
    go [] _ = _
    go (c1 : c1s) c2s = _
```

As described in the instructions, we want to multiply each coefficient in the
first polynomial by each coefficient in the second.

In the base case of `go`, consider what `0 *` anything should be. In the
recursive case, use `map` to multiply every coefficient in `c2s` by `c1`, and
then in the recursive call, shift `c2s` by adding a `0` to the front.
