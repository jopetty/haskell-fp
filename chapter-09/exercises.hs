{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Use list literal pattern" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use notElem" #-}
module Lists where

import Data.Bool
import Data.Char

-- 9.3 Pattern matching on lists

-- Quick n' dirty implementation of `head` and `tail`

myHead (x : _) = x

myTail (_ : xs) = xs

-- However, these will fail on empty lists. To get around this, we need
-- to wrap the return value in a Maybe monad and handle the exceptional
-- cases explicitly

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : []) = Nothing  -- could say [_] instead, but (_ : []) is clearer
safeTail (_ : xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

-- Exercise: EnumFromTo

-- Write your own enumFromTo definitions for the types provided. Do not use range
-- syntax to do so. It should return the same results as if you did [start..stop]. 
-- Replace the undefined, a value that results in an error when evaluated, with your 
-- own definition.

eftBool :: Bool -> Bool -> [Bool]
eftBool a b
    | a > b = []
    | otherwise = [a, succ a]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b
    | a > b = []
    | otherwise = [a] ++ eftOrd (succ a) b

eftInt :: Int -> Int -> [Int]
eftInt a b
    | a > b = []
    | otherwise  = [a] ++ eftInt (succ a) b

eftChar :: Char -> Char -> [Char]
eftChar a b
    | a > b = []
    | otherwise = [a] ++ eftChar (succ a) b


-- Excercses: Thy Fearful Symmetry

-- 1. Using takeWhile and dropWhile, write a function that takes a string and returns a 
--    list of strings, using spaces to separate the elements of the string into words, 
--    as in the following sample:
--      Prelude> myWords "sheryl wants fun"
--      ["sheryl", "wants", "fun"]

myWords :: String -> [String]
myWords s
    | s == "" = []
    | otherwise = 
        let word = takeWhile (/= ' ') s
            rest = dropWhile (== ' ') (dropWhile (/= ' ') s)
        in word : myWords rest


-- 2. Next, write a function that takes a string and returns a list of strings, using 
--    newline separators to break up the string, as in the following (your job is to 
--    fill in the undefined function):

myLines :: String -> [String]
myLines s
    | s == "" = []
    | otherwise =
        let first = takeWhile (/= '\n') s
            remaining = dropWhile (== '\n') (dropWhile (/= '\n') s)
        in first : myLines remaining

-- 3. Now, let’s look at what those two functions have in common. Try writing a new 
--    function that parameterizes the character you’re breaking the string argument on 
--    and rewrite myWords and myLines using that parameter.

splitOn :: String -> Char -> [String]
splitOn s c
    | s == "" = []
    | otherwise =
        let first = takeWhile (/= c) s
            rest = dropWhile (== c) (dropWhile (/= c) s)
        in first : splitOn rest c


-- Exercises: Comprehend thy lists

-- Prelude> mySqr = [x^2 | x <- [1..10]]
-- Prelude> [x | x <- mySqr, rem x 2 == 0]

-- This will return every even square number from 1 to 10^2, so [4, 16, 36, 64, 100]

-- [(x, y) | x <- mySqr,
--           y <- mySqr,
--           x < 50, y > 50]

-- This will return every tuple where (a) both elements are square, (b) the first value
-- is less than fifty, and (c) the second value is between 50 and 100, so 
--   {1,4,9,16,25,36,49} x {64,81,100}

-- take 5 [ (x, y) | x <- mySqr,
--                   y <- mySqr,
--                   x < 50, y > 50 ]

-- This will return the first five elements of the previous question; since the 
-- expressions will be evaluated first by values of x and then by y, it will thus return
--   [(1, 64), (1, 81), (1,100), (4, 64), (4,81)]

-- List comp with strings

acro xs = [x | x <- xs, elem x ['A'..'Z']]

myString xs = [x | x <- xs, elem x "aeiou"]
-- This will filter out everything that isn't a vowel from xs


-- Exercises: Square cube

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

-- 1. First write an expression that will make tuples of the outputs of mySqr and 
--    myCube.

sqrCubeTuple = [(x,y) | x <- mySqr, y <- myCube]

-- 2. Now, alter that expression so that it only uses the x and y values that are less 
--    than 50.
sqrCubeTuple' = [(x,y) | x <- mySqr, x < 50, y <- myCube, y < 50]

-- 3. Apply another function to that list comprehension to determine how many tuples 
--    inhabit your output list.

-- length sqrCubeTuple == 25
-- length sqrCubeTuple' == 15


-- Spines, values, and normal forms

-- "normal form" -> expression is fully evaluated
-- "weak head normal form" -> expression is evaluated as far as is necessary to reach 
--    a data constructor
-- NF => WHNF, but not the other way around, so WHNF is a struct superset of NF

-- `(1, 2)` is NF
-- `(1, 1 + 1)` is WHNF but not NF, since `1+1` has not yet been evaluated
-- `\x -> x * 10` is NF, since no reduction is possible until `x -> ...` has 
--     been applied
-- `"Papu" ++ "chon"` is neither WHNF nor NF, since nothing has been evaluated
-- `(1, "Papu" ++ "chon")` is WHNF but not NF, since the outer part is evaluated but
--     the second element is not
-- `num :: [Int]; num = [1,2,3]` is NF
-- `num :: [Int]; num = [1..10]` is WHNF but not NF

-- functions can be strict on a spine, or also strict on values.
-- `length` is spine-strict but not value strict:
--     > length [1, undefined]
--     2

length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs


-- Exercises: Bottom madness

-- A. Will it blow up?
-- Will the following expressions return a value or be ⊥?
-- 1. `[x^y | x <- [1..5], y <- [2, undefined]]` -> ⊥
-- 2. `take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]` -> [1]
-- 3. `sum [1, undefined, 3]` -> ⊥
-- 4. `length [1, 2, undefined]` -> 3
-- 5. `length $ [1, 2, 3] ++ undefined` -> ⊥
-- 6. `take 1 $ filter even [1, 2, 3, undefined]` -> [2]
-- 7. `take 1 $ filter even [1, 3, undefined]` -> ⊥
-- 8. `take 1 $ filter odd [1, 3, undefined]` -> [1]
-- 9. `take 2 $ filter odd [1, 3, undefined]` -> [1,3]
-- 10. `take 3 $ filter odd [1, 3, undefined]` -> ⊥

-- B. Intermission: Is it in normal form?
-- For each expression below, determine whether its in:
--   1. NF (=? WHNF)
--   2. WHNF only
--   3. Neither

-- 1. `[1, 2, 3, 4, 5]` is NF
-- 2. `1 : 2 : 3 : 4 : _` is WHNF? Yes
-- 3. `enumFromTo 1 10` is neither? Yes
-- 4. `length [1, 2, 3, 4, 5]` is neither? Yes
-- 5. `sum (enumFromTo 1 10)` is WHNF? WRONG, neither
-- 6. `['a'..'m'] ++ ['n'..'z']` is WHNF? WRONG, neither
-- 7. `(_, 'b')` is WHNF

-- Exercises: More bottoms

-- 1. Will the following expression return a value or be ⊥?
--   `take 1 $ map (+1) [undefined, 2, 3]` -> ⊥

-- 2. Will the following expression return a value?
--   `take 1 $ map (+1) [  1, undefined,3]` -> [2]

-- 3. Will the following expression return a value?
--   `take 2 $ map (+1) [  1, undefined,3]` -> ⊥

-- 4. What does the following mystery function do? What is its type? Describe it (to 
--    yourself or a loved one) in standard English and then test it out in the REPL to 
--    make sure you are correct:
--    `itIsMystery xs = map (\ x -> elem x "aeiou") xs`
--    It returns a list of booleans denoting whether each character in a string is a 
--    vowel. It's type is `String -> [Bool]`

-- 5. What will be the result of the following functions:
--    a) `map (^2) [1..10]` -> first ten squares
--    b) `map minimum [[1..10], [10..20], [20..30]]` -> [1, 10, 20]
--    c) `map sum [[1..5], [1..5], [1..5]]` -> [15, 15, 15]

-- 6. Back in Chapter 7, you wrote a function called foldBool. That function exists in 
--    a module known as Data.Bool and is called bool. Write a function that does the 
--    same (or similar, if you wish) as the map if-then-else function you saw above but 
--    uses bool instead of the if-then-else syntax. Your first step should be bringing 
--    the bool function into scope by typing import Data.Bool at your REPL prompt.


-- Reference func: `map (\x -> if x == 3 then (-x) else (x)) [1..10]`

negateThree = map (\x -> bool x (-x) (x == 3))

-- Exercises: Filtering

-- 1. Given the above, how might we write a filter function that would give us all the 
--    multiples of 3 out of a list from 1–30?

isTriple xs = filter (\x -> rem x 3 == 0) xs
-- `isTriple [1..30]` = [3,6,9,12,15,18,21,24,27,30]

-- 2. Recalling what we learned about function composition, how could we compose the 
--    above function with the length function to tell us how many multiples of 3 there 
--    are between 1 and 30?
howManyTriples xs = length $ isTriple xs
-- `howManyTriples [1..30]` = 10

-- 3. Next, we’re going to work on removing all articles (“the,” “a,” and “an”) from 
--    sentences. You want to get to something that works like this:
--      Prelude> myFilter "the brown dog was a goof" 
--      ["brown","dog","was","goof"]
--    You may recall that earlier in this chapter, we asked you to write a function 
--    that separates a string into a list of strings by separating them at spaces. That 
--    is a standard library function called words. You may consider starting this 
--    exercise by using words (or your own version, of course).
myFilter xs = filter (\x -> not $ elem x ["the", "a", "an"]) (words xs)

-- Zipping exercises:
-- 1. Write your own version of zip, and ensure it behaves the same as the original:
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = [(a,b)] ++ zip' as bs

-- 2. Do what you did for zip but now for zipWith:
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (a:as) (b:bs) = [f a b] ++ zipWith' f as bs

-- 3. Rewrite your zip in terms of the zipWith you wrote.
zip'' :: [a] -> [b] -> [(a,b)]
zip'' = zipWith' (,)

-- 9.12 Chapter Exercises

-- Data.Char

-- 1. Query the types of `isUpper` and `toUpper`
--    `:t isUpper`: isUpper :: Char -> Bool
--    `:t toUpper`: toUpper :: Char -> Char

-- 2. Given the following behaviors, which would we use to write a function that 
--    filters all the uppercase letters out of a String? Write that function such that, 
--    given the input "HbEfLrLxO", your function will return "HELLO".

-- You'd want `isUpper`:
onlyUppers :: String -> String
onlyUppers = filter isUpper

-- 3. Write a function that will capitalize the first letter of a string and return the 
--    entire string. For example, if given the argument "julie", it will return "Julie".
upperFirst :: String -> String
upperFirst s = [toUpper $ head s] ++ tail s

-- 4. Now make a new version of that function that is recursive, such that if you give 
--    it the input "woot", it will holler back at you "WOOT". The type signature won’t 
--    change, but you will want to add a base case.
upperAll :: String -> String
upperAll "" = ""
upperAll s = upperFirst [head s] ++ upperAll (tail s)

-- 5. To do the final exercise in this section, we’ll need another standard function 
--    for lists called head. Query the type of head, and experiment with it to see what 
--    it does. Now write a function that will capitalize the first letter of a String 
--    and return only that letter as the result.
upperOnlyFirst :: String -> Char
upperOnlyFirst s = head (upperFirst s)

upperOnlyFirst' :: String -> Char
upperOnlyFirst' = toUpper . head

-- Cipher
-- see cipher.hs

-- Standard functions
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem a ls = myOr (map (==a) ls)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a:as) = myReverse as ++ [a]

squish :: [[a]] -> [a]
squish [] = []
squish (a:as) = a ++ squish as

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs
    | null xs        = undefined
    | length xs == 1 = head xs
    | length xs == 2 = if f (head xs) (head (tail xs)) == GT then head xs else head (tail xs)
    | otherwise = if f (head xs) (head (tail xs)) == GT then myMaximumBy f ([head xs] ++ tail (tail xs)) else myMaximumBy f (tail xs)