module Exercises where

-- data types by example

data Weather
  = Sunny
  | Cloudy
  | Windy
  | Rainy
  | Snowy
  deriving (Show)

data Point = Point Int Int
  deriving (Show)

data WeatherRequest
  = ByLocation String
  | ByCoordinate Point
  deriving (Show)

data WeatherResult
  = Valid Weather
  | Invalid
  deriving (Show)

-- recursive data types

data Tree
  = Leaf
  | Node Tree Int Tree
  deriving (Show)

tree :: Tree
tree = Node Leaf 1 (Node Leaf 2 Leaf)

{- Count the number of leaves in the tree. For example,
   [numLeaves tree] should be [3]. -}

countLeaves :: Tree -> Int
countLeaves Leaf = 1
countLeaves (Node l _ r) = countLeaves l + countLeaves r


{- Check whether a number is in the tree. For example,
   [find 1 tree] and [find 2 tree] should be [True], while
   [find 3 tree] should be [False]. -}

find :: Int -> Tree -> Bool
find _ Leaf = False
find n (Node l x r)
  | n == x = True
  | otherwise = find n l || find n r


{- Add three to every node value in the tree. For example,
   [add3 tree] should be [Node Leaf 4 (Node Leaf 5 Leaf)]. -}

add3 :: Tree -> Tree
add3 Leaf = Leaf
add3 (Node l x r) = Node (add3 l) (x + 3) (add3 r)