-- exercises.hs

module Exercises where

-- 1. Which (two or more) of the following are equivalent?
--  a) mTh x y z = x * y * z
--  b) mTh x y = \z -> x * y * z
--  c) mTh x = \y -> \z -> x * y * z
--  d) mTh = \x -> \y -> \z -> x * y * z
-- All equivalent

-- 2, The type of mTh (above) is Num a => a -> a -> a -> a. 
-- Which is the type of mTh 3?
--  a) Integer -> Integer -> Integer 
--  b) Num a => a -> a -> a -> a
--  c) Num a => a -> a
--  d) Num a => a -> a -> a
-- (d)

-- 3. Next, we’ll practice writing anonymous lambda syntax.
--  a) Rewrite the f function in the where clause:
-- addOneIfOdd n = case odd n of 
--     True -> f n
--     False -> n
--     where f n = n + 1
addOneIfOdd n = case odd n of 
    True -> f n
    False -> n
    where f = \n -> n + 1
--  b) Rewrite the following to use anonymous lambda syntax:
--      addFive x y = (if x > y then y else x) + 5
addFive = (\x -> \y -> (if x > y then y else x) + 5)
--  c) Rewrite the following so that it doesn’t use anonymous lambda syntax:
--      mflip f = \x -> \y -> f y x
mflip f x y = f y x


-- Section 7.4 
--  Variety pack
-- 1. Given the following declarations:
k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2)) 
k3 = k (3, True)
--  a) what is the type of `k`?
--  b) What is the tpye of k2? is it the same as k1 or k3?
--  c) Of k1, k2, k3, which will return the number 3 as the result?
-- (a): k :: (a, b) -> a
-- (b) k2 :: String. Not the same as k1 :: Integer or k2 :: Integer
-- (c) k1 and k3 will return 3

-- 2. Fill in the definition of the following function:
f :: (a, b, c)
    -> (d, e, f)
    -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

-- Section 7.5 Case practice

-- 1. The following should return x when x is greater than y:
--  functionC x y = if (x > y) then x else y
functionC x y = 
    case x > y of
        True  -> x
        False -> y

-- 2. The following will add 2 to even numbers and otherwise simply return the input 
--    value:
--      ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2 x =
    case (mod) x 2 == 0 of
        True  -> x + 2
        False -> x

-- 3. The following compares a value, x, to 0 and returns an indicator for whether x is 
--  a positive number or negative number. What if x is 0? You may need to play with the 
--  compare function a bit to find what to do:
--      nums x =
--          case compare x 0 of
--              LT -> -1 
--              GT -> 1
nums x = 
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0


-- Section 7.5 Artful Dodgy
-- Given these defs, what do the following evaluate to?
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

-- 1. dodgy 1 0
d1 = dodgy 1 0 == 1

-- 2. dodgy 1 1
d2 = dodgy 1 1 == 11

-- 3. dodgy 2 2
d3 = dodgy 2 2 == 22

-- 4. dodgy 1 2
d4 = dodgy 1 2 == 21

-- 5. dodgy 2 1
d5 = dodgy 2 1 == 12

-- 6. oneIsOne 1
d6 = oneIsOne 1 == 11

-- 7. oneIsOne 2
d7 = oneIsOne 2 == 21

-- 8. oneIsTwo 1
d8 = oneIsTwo 1 == 21

-- 9. oneIsTwo 2
d9 = oneIsTwo 2 == 22

-- 10. oneIsOne 3
d10 = oneIsOne 3 == 31

-- 11. oneIsTwo 3
d11 = oneIsTwo 3 == 23


myAbs :: Integer -> Integer
myAbs x
    | x < 0     = (-x)
    | otherwise = x

bloodNa :: Integer -> String
bloodNa x 
    | x < 135   = "too low"
    | x > 145   = "too high"
    | otherwise = "just right"


isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c 
    | a^2 + b^2 == c^2 = "RIGHT ON"
    | otherwise        = "not right"

dogYrs :: Integer -> Integer
dogYrs x
    | x <= 0    = 0
    | x <= 1    = x * 15
    | x <= 2    = x * 12
    | x <= 4    = x * 8
    | otherwise = x * 6


-- Guard duty
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x 
    | y >= 0.9 ='A' 
    | y >= 0.8 ='B' 
    | y >= 0.7 ='C' 
    | y >= 0.59 = 'D' 
    | otherwise = 'F' 
    where y = x / 100

-- 1. Lol, everyone gets an F
-- 2. Lol, everyone gets a C
-- 3. What does the following function return?
pal xs
 | xs == reverse xs = True 
 | otherwise = False
-- it returns (b), True when xs is a palindrome
-- 4. What types of arguments can pal take?
-- any list of equatable things
-- 5. What is the type of the function pal?
-- `pal :: Eq a => [a] -> Bool` 
-- 6. What does the following function return?
numbers x
    | x < 0  = -1
    | x == 0 = 0 
    | x > 0  = 1
-- (c) an indication of whether the arg is positive, negative, or zero
-- 7. What types of arguments can numbers take?
-- Anything that's both numeric and ordered
-- 8. What is the type of the function numbers?
-- `numbers :: (Num a, Ord a, Num b) => a -> b`


-- 7.11 Chapter exercises
-- Multiple Choice
-- 1. A polymorphic function:
--      (d) May resolve to values of different types, depending on inputs
-- 2. Two functions named f and g have types Char -> String and String -> [String], /
--    respectively. The composed function g . f has the type:
--      (b) `Char -> [String]`
-- 3. A function f has the type Ord a => a -> a -> Bool,and we apply
--      it to one numeric value. What is the type now?
--      (d) (Ord a, Num a) => a -> Bool
-- 4. A function with the type (a -> b) -> c:
--      (b) Is a higher-order function.
-- 5. Given the following definition of f, what is the type of f True?
-- f :: a -> a
-- f x = x
--      (a) f True :: Bool

-- Let's write code
-- 1. The following function returns the tens digit of an integral argument:
tensDigit :: Integral a => a -> a 
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10
--  (a) First, rewrite it using divMod.
tensDigit' :: Integral a => a -> a
tensDigit' x = snd . divMod (fst . divMod x $ 10) $ 10
-- (b) Yeah it's the same type
-- (c) Make it get the hundreds digit
hunsD x = snd . divMod (fst . divMod x $ 100) $ 10


-- 2. Implement the following function of the type a -> a -> Bool -> a once using a 
-- case expression and once with a guard:
foldBool :: a -> a -> Bool -> a 
-- foldBool =
--     error
--     "Error: Need to implement foldBool!"
foldBool x y z =
    case z of
        False  -> x
        True -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y z
    | not z = x
    | z     = y

foldBool3 :: a -> a -> Bool -> a 
foldBool3 x _ False = x 
foldBool3 _ y True = y

-- 3. Fill in the definition. Note that the first argument to our function is also a 
--    function that can be applied to values. Your second argument is a tuple, which 
--    can be used for pattern matching:
g :: (a -> b) -> (a, c) -> (b, c) 
g f (a, c) = (f a, c)

-- 4. For this next exercise, you’ll experiment with writing point-free versions of 
--    existing code. This involves some new information, so read the following 
--    explanation carefully.
-- Type classes are dispatched by type. `Read` is a type class like `Show`, but it is 
-- the dual or “opposite” of `Show`. In general, the `Read` type class isn’t something 
-- you should plan to use, but this exercise is structured to teach you something about 
-- the interaction between type classes and types.
-- The function `read` in the `Read` type class has the type:
--  read :: Read a => String -> a
-- Notice a pattern?
--  read :: Read a => String -> a
--  show :: Show a => a -> String
-- Type the following code into a source file. Then load it, and run it in GHCi to make 
-- sure you understand why the evaluation results in the answers you see:
-- (See arith4.hs)

-- 5. Next, write a point-free version of roundTrip. (n.b., this refers to the function 
--    definition, not to its application in main.)
roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show
