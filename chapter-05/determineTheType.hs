-- determineTheType.hs

{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- simple example
example = 1
-- example :: Num p => p


-- 1. All function applications return a value. Determine the values returned by these function applications and the types of those values:
-- a) (* 9) 6
-- b) head [(0,"doge"),(1,"kitteh")]
-- c) head [(0 :: Integer ,"doge"),(1,"kitteh")]
-- d) if False then True else False
-- e) length [1, 2, 3, 4, 5]
-- f) (length [1, 2, 3, 4]) > (length "TACOCAT")

aVar = (* 9) 6
-- aVar :: Num a => a

bVar = head [(0,"doge"),(1,"kitteh")]
-- bVar :: Num a => (a, String)

cVar = head [(0 :: Integer ,"doge"),(1,"kitteh")]
-- cVar :: (Integer, String)

dVar = if False then True else False
-- dVar :: Bool

eVar = length [1, 2, 3, 4, 5]
-- eVar :: Int

fVar = (length [1, 2, 3, 4]) > (length "TACOCAT")
-- fVar :: Bool

-- 2. Given:
x1 = 5 
y1 = x1 + 5 
w = y1 * 10
-- What is the type of `w`?
-- w :: Num a => a

-- 3. Given:
x2 = 5
y2 = x2 + 5
z y2 = y2 * 10
-- What is the type of `z`?
-- z :: Num a => a -> a

-- 4. Given:
x3 = 5
y3 = x3 + 5
f = 4 / y3
-- What is the type of `f`?
-- f :: Fractional a => a -> a

-- 5. Given:
x4 = "Julie"
y4 = " <3 "
z4 = "Haskell"
f4 = x4 ++ y4 ++ z4
-- What is the type of `f4`?
-- f4 :: String


-- SECTION: Does it compile?

-- 1.
-- bigNum = (^) 5 $ 10 
-- wahoo = bigNum $ 10
-- Doesn't compile, but we can change it to:

bigNum = (^) 5 $ 10 
wahoo = (^) bigNum $ 10

-- 2.
-- x = print
-- y = print "woohoo!"
-- z = x "hello world"
-- Compiles fine:
x = print
y = print "woohoo!"
z = x "hello world"

-- 3. 
-- a = (+)
-- b = 5
-- c = b 10
-- d = c 200
-- Doesn't compile, but we can change it to
a = (+)
b = 5
c = a b
d = c 200

-- 4.
-- a = 12 + b
-- b = 10000 * c
-- Doesn't compile, but we can change it to
a = 12 + b
b = 100000 * c
c = 3

-- SECTION: Type variable or specific type constructor
-- 2. Categorize each component of this type signature as described in the 
--    previous example:
--    f :: zed -> Zed -> Blah
--         [0]    [1]    [2]
-- [0]: fully polymorphic type variable
-- [1]: concrete type constructor
-- [2]: concrete type constructor

-- 3. Categorize each component of this type signature:
--    f :: Enum b => a -> b -> C
--                   [0]  [1]  [2]
-- [0]: fully polymorphic type variable
-- [1]: constrained type variable (must derive Enum)
-- [2]: concrete type constructor

-- 4. Categorize each component of this type signature:
--    f :: f -> g -> C
--         [0]  [1]  [2]
-- [0]: fully polymorphic type variable
-- [1]: fully polymorphic type variable
-- [2]: concrete type constructor

-- SECTION: Write a type signature
-- 1. While we haven’t fully explained this syntax yet, you saw it in Chapter 2 and as 
-- a solution to an exercise in Chapter 4. This syntax is a way of destructuring a 
-- single element of a list by pattern matching:
--      functionH :: [a] -> a
--      functionH (x:_) = x

-- 2.
--      functionC :: Ord a => a -> a -> Bool
--      functionC x y =
--          if (x > y) then True else False

-- 3. 
--      functionS :: (a, b) -> b
--      functionS (x, y) = y

-- SECTION: Given a type, write the function
-- 1. There is only one function definition that type checks and doesn’t go into an 
--    infinite loop when you run it:
--       i :: a -> a
--       i = undefined
i :: a -> a
i x = x

-- 2. There is only one version that works:
--      c :: a -> b -> a
--      c = undefined
c :: a -> b -> a
c x y = x

-- 3. Given alpha equivalence, are the variables c'' and c (from the previous exercise) 
--    the same thing?
--      c'' :: b -> a -> b
--      c'' = ?
-- Yes, they are the same thing.

-- Only one version works:
--      c' :: a -> b -> b
--      c' = undefined
c' :: a -> b -> b
c' x y = y

-- 4. There are multiple possibilities, at least two of which you’ve seen in previous 
--    chapters:
--      r :: [a] -> [a]
--      r = undefined
r :: [a] -> [a]
r x = x
r x = tail x

-- 6. Only one version will type check:
--      co :: (b -> c) -> (a -> b) -> a -> c
--      co = undefined
co :: (b -> c) -> (a -> b) -> a -> c
c f g = f . g

-- 7. One version will type check:
--      a :: (a -> c) -> a -> a
--      a = undefined
a :: (a -> c) -> a -> a
a f x = x

-- 8. One version will type check:
--      a' :: (a -> b) -> a -> b
--      a' = undefined
a' f x = f x