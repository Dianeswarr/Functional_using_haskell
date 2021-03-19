
-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module Assignment3(Fruit(..),
            sumPrice,
            BSTree(Void,BSNode),
            subTree,
            Tree(..),
            count,labels,height,
            (++),elem,last,reverse,filter) where

import Prelude hiding ((++),elem,last,reverse,filter)
-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

--import sort list
import Data.List (sort)

{- Exercise 1: Fruit
   =================

   1. We consider three kinds of fruit: apples, bananas and lemons. Bananas and
      apples are sold by the kilogram, but a lemon always has the same price,
      regardless of weight.

      Give a datatype declaration for the type `Fruit'. The datatype should
      reflect that there are three kinds of fruit, so the datatype definition
      should have three cases. For apples (`Apple') and bananas (`Banana') there
      should be an associated weight in kilogram (given as a value of type
      `Double'). For lemons (`Lemon') there should be an associated number of
      units (given as a value of type `Integer').
 -}

{- (Remember to provide a datatype representation in accordance with our Coding
   Convention.)
 -}
data Fruit = Apple Double | Banana Double | Lemon Integer  
  deriving (Eq,Show)  -- do not modify this line

{- 2. Define a function

        sumPrice :: [Fruit] -> Double -> Double -> Double -> Double

      This function should take a list of fruit, the price of apples (per
      kilogram), bananas (per kilogram), and the price of lemons (per unit), and
      return the total cost of the items in the list.
 -}

{- (Remember to provide a complete function specification.)
 -}
{- sumPrice
     Returns the total cost of all the fruits
     PRE:  all the prices are positive numbers
     RETURNS: The total cost of all the fruits
     EXAMPLES: sumPrice [Apple 2.0, Banana 2.5, Lemon 5] 1 2 3 = 22.0
               sumPrice [Apple 2.0, Banana 2.5, Lemon 5] 3 2 1 = 16.0
               sumPrice [Apple 2.0, Banana 2.5, Lemon 5] 0 0 0 = 0.0
               sumPrice [Apple 2.0, Lemon 5, Banana 2.5, Apple 1.0 , Lemon 5] 3.0 3.5 0.5 = 22.75
-}

--INVARIANT:?

sumPrice :: [Fruit] -> Double -> Double -> Double -> Double
-- (If your function is recursive, remember to provide a variant.)
sumPrice [] priceA priceB priceL = 0

sumPrice (Apple x : xs) priceA priceB priceL = x * priceA + sumPrice xs priceA priceB priceL

sumPrice (Banana x : xs) priceA priceB priceL = x * priceB + sumPrice xs priceA priceB priceL

sumPrice (Lemon x : xs) priceA priceB priceL = fromInteger x * priceL + sumPrice xs priceA priceB priceL
--sumPrice (x : xs) priceA priceB priceL | eq x Fruit  = 1.0

{- Exercise 2: Binary Search Trees
   ===============================

   The nodes of a binary search tree are ordered from left to right according to
   their key value. Consider the following datatype declaration of binary search
   trees:
 -}

{- Binary search trees with Integer labels

   Void represents an empty tree. BSNode l x r represents a tree with
   left subtree l, root label x, and right subtree r.

   INVARIANT: in every tree of the form BSNode l x r, all labels in l
     are < x, and all labels in r are > x.
 -}
data BSTree = Void | BSNode BSTree Integer BSTree  -- do not modify this line
  deriving (Eq,Show)  -- do not modify this line

{- Define a function

     subTree :: Integer -> Integer -> BSTree -> BSTree

   such that `subTree a b t' yields a binary search tree containing the keys in
   `t' that are greater than or equal to `a' but smaller than `b'.

   Example: Assume that `t' is defined as follows:

     t = BSNode (BSNode (BSNode Void 0 (BSNode Void 2 Void))
                        3
                        (BSNode Void 5 Void))
                6
                (BSNode Void
                        7
                        (BSNode Void 8 (BSNode Void 9 Void)))

   We now have

     subTree 5 8 t == BSNode (BSNode Void 5 Void)
                             6
                             (BSNode Void 7 Void)

   (The tree produced by your solution might be shaped differently, but it
   should also contain the keys 5, 6 and 7, and no other keys.) We also have

     subTree 10 20 t == Void

   This question can (and should) be solved without introducing intermediate
   data structures (such as lists).
 -}

 

{- (Remember to provide a complete function specification.)
 -}
{- subTree
     Return the binary tree st that all the values are between the range of [a, b) of a given tree t
     PRE:  all values needs to be integer
     RETURNS: The binary tree st that all the values are between the range of [a, b) of a given tree t
     EXAMPLES: if t 
     t = BSNode (BSNode (BSNode Void 0 (BSNode Void 2 Void))
                        3
                        (BSNode Void 5 Void))
                6
                (BSNode Void
                        7
                        (BSNode Void 8 (BSNode Void 9 Void)))

      then subTree 5 8 t == BSNode (BSNode Void 5 Void)
                             6
                             (BSNode Void 7 Void)

            subTree 0 10 = BSNode (BSNode (BSNode Void 0 (BSNode Void 2 Void))
                        3
                        (BSNode Void 5 Void))
                6
                (BSNode Void
                        7
                        (BSNode Void 8 (BSNode Void 9 Void)))

            subTree 0 0 = Void
            subTree 10 20 = Void
-}


subTree :: Integer -> Integer -> BSTree -> BSTree
--variant = ?
subTree a b Void = Void
subTree a b (BSNode l x r)
		| x >= a && x < b = (BSNode (subTree a b l) x (subTree a b r))
		| x < a = subTree a b r
		| x >= b = subTree a b l
		| otherwise = Void


-- (If your function is recursive, remember to provide a variant.)

{- Exercise 3: Trees
   =================

   1. Define a datatype `Tree a' of (finitely branching) labeled trees. Each
      node (`Node') carries a label (of polymorphic type `a') and may have an
      arbitrary (non-negative) number of children. Different nodes may have
      different numbers of children. Each tree has at least one node (so there
      should be no `Void' case in your datatype definition).

      See [ https://en.wikipedia.org/wiki/Tree_%28data_structure%29 ] for
      further definitions and some hints.
 -}

{- (Remember to provide a datatype representation in accordance with our Coding
   Convention.)
 -}

data Tree a = Node a [Tree a]   -- remove `Tree' and write your datatype declaration here
  deriving (Eq,Show)  -- do not modify this line

{- 2. Define functions to

      (a) compute the number of nodes in such a tree:

            count :: Tree a -> Integer

      (b) compute the list of all node labels in such a tree:

            labels :: Tree a -> [a]

      (c) compute the height of such a tree:

            height :: Tree a -> Integer

      Hint: a possible solution is to use helper functions and mutual recursion.
      Another solution uses higher-order functions such as `map'.
 -}

{- (Remember to provide a complete function specification.)
 -}

{- count
     Counts all the leaves (nodes) of a Tree
     PRE: All the leaves needs to be of polymorphic type a
     RETURNS: The number of leaves or Nodes of a tree
     EXAMPLE: 
              count (Node 2 []) = 1

              count (Node 2 [Node 1 []]) = 2

              count (Node 1 []) = 1

              count (Node 3 [Node 1 [], (Node 1 [Node 2 [], Node 3 []]), Node 3 []] = 6

              count (Node 3 [Node 1 [], Node 1 [Node 2 [], Node 3 []], Node 3 [], Node 5 []]) = 7

              count (Node 3 [Node 1 [], Node 1 [Node 2 [], Node 3 []], Node 3 [], Node 3 [Node 1 , Node 2 []]]) = 9

-}
-- variant: ?
count :: Tree a -> Integer
count (Node _ []) = 1
count (Node _ lst) = (+) 1 $ sum $ map count lst

{- (Remember to provide a complete function specification.)
 -}
{- labels
     returns a list [a] of a given Tree a 
     PRE: All the leaves needs to be of polymorphic type a
     RETURNS: a list with all the leaves and nodes
     EXAMPLE: 
              labels (Node 1 [Node 2 [], (Node 3 []), Node 4 [], (Node 5 [Node 6 [], Node 7 [], (Node 8 [Node 9 [], Node 10 []])])]) = [1,2,3,4,5,6,7,8,9,10]

              labels Node 1 [Node 2 [], Node 3 []] = 
              [1,2,3]

              count (Node 1 []) = [1]

              count (Node 3 [Node 1 [], Node 1 [Node 2 [], Node 3 []], Node 3 []] = [3,1,1,2,3,3]

-}



-- variant: ?

labels :: Tree a -> [a]
labels (Node x []) = [x]
labels (Node x lst) = x:(concat $ map labels lst)

{- (Remember to provide a complete function specification.)
 -}
height :: Tree a -> Integer
height (Node a []) = 0
height (Node a lst) = (+) 1 $ maximum $ map height lst


{- Exercise 4: Higher-Order Functions
   ==================================

   You have already seen recursive definitions for the functions `(++)', `elem',
   `last', `reverse' and `filter' in class. Now your task is to give
   non-recursive definitions for these functions, using the higher-order
   functions `foldl' or `foldr'. Define

   1. (++) :: [a] -> [a] -> [a]
   2. elem :: Eq a => a -> [a] -> Bool
   3. last :: [a] -> a
   4. reverse :: [a] -> [a]
   5. filter :: (a -> Bool) -> [a] -> [a]

   In all cases, the definition using foldl or foldr should be quite simple
   (i.e., a single line of code).
 -}
{- (Remember to provide a complete function specification.)
 -}
(++) :: [a] -> [a] -> [a]
(++) a b = foldr (:) b a  -- remove `= undefined' and define your function here


{- (Remember to provide a complete function specification.)
 -}
elem :: Eq a => a -> [a] -> Bool
--elem = undefined
elem a b = foldl (\xs x-> if x == a then (True) else (False || xs)) (False) b

{- (Remember to provide a complete function specification.)
 -}
last :: [a] -> a
last a = foldl (\xs x -> x) undefined a



{- (Remember to provide a complete function specification.)
 -}
reverse :: [a] -> [a]
reverse a = foldl (\xs x-> x : xs) [] a  -- remove `= undefined' and define your function here

{- (Remember to provide a complete function specification.)
 -}
filter :: (a -> Bool) -> [a] -> [a]
filter a b = foldr (\x xs -> if a x then x : xs else xs) [] b