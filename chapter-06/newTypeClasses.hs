-- newTypeClasses.hs

module NewTypeClasses where

import Data.List

data Trivial =
    Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True

data DayOfWeek = 
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
    deriving (Ord, Show)

-- day of week and numerical day of month
data Date =
    Date DayOfWeek Int

instance Eq DayOfWeek where
    (==) Mon Mon   = True
    (==) Tue Tue   = True
    (==) Weds Weds = True
    (==) Thu Thu   = True
    (==) Fri Fri   = True
    (==) Sat Sat   = True
    (==) Sun Sun   = True
    (==) _ _       = False

instance Eq Date where
    (==) (Date weekday dayOfMonth)
         (Date weekday' dayOfMonth') = 
            weekday == weekday'
            && dayOfMonth == dayOfMonth'

f :: Int -> Bool
f 1 = True
f 2 = True
f 3 = True
f _ = False

data Identity a = 
    Identity a

instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'

data NoEq = NoEqInst deriving Show


-- Exercises: Eq instances
-- 1. It’s not a typo, we’re just being cute with the name:
data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn a)
         (TisAn a') = 
            a == a'

-- 2.
data TwoIntegers = 
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two a b)
         (Two a' b') = 
            a == a'
            && b == b'

-- 3.
data StringOrInt = 
    TisAnInt     Int
    | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt a)
         (TisAnInt a') = 
            a == a'
    (==) (TisAString a)
         (TisAString a') = 
            a == a'
    (==) _ _ = False

-- 4.
data Pair a =
    Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair a b)
         (Pair a' b') = 
            a == a'
            && b == b'

-- 5.
data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b)
         (Tuple a' b') = 
            a == a'
            && b ==  b'

-- 6.
data Which a = 
    ThisOne a
    | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne a)
         (ThisOne a') = 
            a == a'
    (==) (ThatOne a)
         (ThatOne a') = 
            a == a'
    (==) _ _ = False

-- 7.
data EitherOr a b = 
    Hello a
    | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a)
         (Hello a') = 
            a == a'
    (==) (Goodbye b)
         (Goodbye b') = 
            b == b'
    (==) _ _ = False

-- Exercise: Tuple experiment Look at the types given for quotRem and divMod. What do 
-- you think those functions do? Test your hypotheses by playing with them in the 
-- REPL. We’ve given you a sample to start with below:
--  Prelude> ones x = snd (divMod x 10)

-- ghci> :t divMod
-- divMod :: Integral a => a -> a -> (a, a)

-- `divMod x y` returns a tuple (a, b) where a = `div x y` and b = `mod x y`
-- `quotRem x y` returns a tuple (a, b) where a = `quot x y` and b = `rem x y`

-- Put on your thinking cap:  Why didn’t we need to make the type of the function we //
-- wrote require both type classes? Why didn’t we have to do this:
--  f :: (Num a, Fractional a) => a -> a -> a

-- We don't need to do this because `Fractional` inherits from `Num`, so 
-- `Fractional a => a` guarantees we already have `Num a => a`. 


check' :: Ord a => a -> a -> Bool
check' a a' = a == a'

-- Exercises: Will they work?

-- 1. 
-- max (length [1, 2, 3])
--     (length [8, 9, 10, 11, 12])
-- Yes, this should work fine. `length` has type `[a] -> Int`, and `Int` derives `Ord`,
-- so `max a b` is well-defined and returns `5`.

-- 2. compare (3 * 4) (3 * 5)
-- This should be fine, since `(3 * 4) :: Num` and (3 * 5) :: Num`;
-- Although `Num` doesn't derive `Ord`, the default type `Integer` does, 
-- so this is well-defined and returns `LT`.

-- 3. compare "Julie" True
-- This is not good, since `compare` has signature :: a -> a -> Ordering and
-- `:t "Julie"` is `String` while `:t True` is Bool.

-- 4. (5 + 3) > (3 + 6)
-- This is fine, since `Num`'s default type derives `Ord`, and returns `False`.


-- 
add :: Num a => a -> a -> a 
add x y = x + y

addWeird :: (Ord a, Num a) => a -> a -> a
addWeird x y =
    if x > 1 then x + y else x


-- Chapter Exercises.
-- 1. The Eq class
--  a) includes all types in Haskell.
--  b) is the same as the Ord class.
--  c) makes equality tests possible.
--  d) only includes numeric types.

-- (c)

-- 2. The type class Ord
--  a) allows any two values to be compared. 
--  b) is a subclass of Eq.
--  c) is a superclass of Eq.
--  d) has no instance for Bool.

-- (b)

-- 3. Suppose the type class Ord has an operator >. What is the type of >?
--  a) Ord a => a -> a -> Bool 
--  b) Ord a => Int -> Bool
--  c) Ord a => a -> Char
--  d) Ord a => Char -> [Char]

-- (a)

-- 4. In x = divMod 16 12
--  a) the type of x is Integer.
--  b) the value of x is undecidable.
--  c) the type of x is a tuple. 
--  d) x is equal to 12 / 16.

-- (c)

-- The type class Integral includes
--  a) Int and Integer numbers.
--  b) integral, real, and fractional numbers.
--  c) Schrodinger’s cat.
--  d) only positive numbers.

-- (a)

-- Section: Does it typecheck?

-- 1. Does the following code type check? If not, why not?
-- data Person = Person Bool 
-- printPerson :: Person -> IO ()
-- printPerson person = putStrLn (show person)

-- It does not, because `Person` does not derive `show`. We can fix this by
-- using the default derivation as below:

data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2. Does the following type check? If not, why not?
-- data Mood = Blah | Woot deriving Show
-- settleDown x = 
--     if x == Woot then Blah else x

-- No. We don't require that Mood derives Eq
data Mood = Blah | Woot deriving (Show, Eq)
settleDown x = 
    if x == Woot then Blah else x

-- 3. If you were able to get settleDown to type check:
--  a) What values are acceptable inputs to that function?
--  b) What will happen if you try to run settleDown 9? Why?
--  c) What will happen if you try to run Blah > Woot? Why?

-- (a) `settleDown :: Mood -> Mood`, so the acceptable inputs are `Blah` or `Woot`
-- (b) This will give a runtime error, since `9` is not of type `Mood`
-- (c) Runtime error since we don't stipulate that Mood derives Ord

-- 4. Does the following type check? If not, why not?
-- type Subject = String
-- type Verb = String
-- type Object = String
-- data Sentence =
--     Sentence Subject Verb Object 
--     deriving (Eq, Show)
-- s1 = Sentence "dogs" "drool"
-- s2 = Sentence "Julie" "loves" "dogs"

-- It does not, since `Sentence "dogs" "drool"` is incomplete
type Subject = String
type Verb = String
type Object = String
data Sentence =
    Sentence Subject Verb Object 
    deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"


-- Section: Given a datatype declaration, what can we do?
-- Given the following datatype definitions:
data Rocks =
    Rocks String deriving (Eq, Show)
data Yeah =
    Yeah Bool deriving (Eq, Show)
data Papu =
    Papu Rocks Yeah deriving (Eq, Show)

-- Which of the following will type check? For the ones that don’t
-- type check, why don’t they?
-- 1. phew = Papu "chases" True
--  Error. Papu needs to take Rocks and Yeah, so we'd need to wrap them as
phew = Papu (Rocks "chases") (Yeah True)

-- 2. truth = Papu (Rocks "chomskydoz") (Yeah True)
--  This is fine
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- 3. equalityForall :: Papu -> Papu -> Bool 
--    equalityForall p p' = p == p'
--  This is fine
equalityForall :: Papu -> Papu -> Bool 
equalityForall p p' = p == p'

-- 4. comparePapus :: Papu -> Papu -> Bool
--    comparePapus p p' = p > p'
-- This is not fine, since we don't insist that Papu, Rocks, and Yeah derive Ord

-- Match the types
-- 1.
--  a) i :: Num a => a
--     i = 1
--  b) i :: a


-- i :: a won't work, because we can't convert `1` to `a`

-- 2.
--  a) f :: Float
--     f = 1.0
--  b) f :: Num a => a

-- This won't work, because there's no way to convert `1.0` to `Num a => a`

-- 3. 
--  a) f :: Float
--     f = 1.0
--  b) f :: Fractional a => a
f'' :: Fractional a => a
f'' = 1.0
-- This is fine since `Float` implements `Fractional`

-- 4.
--  a) f :: Float
--     f = 1.0
--  b) f :: RealFrac a => a
-- This is fine, since `Float` implements `RealFrac`
f''' :: RealFrac a => a
f''' = 1.0

-- 5.
--  a) freud :: a -> a
--     freud x = x
--  b) freud :: Ord a => a -> a
-- This is fine?
freud :: Ord a => a -> a
freud x = x

-- 6. 
--  a) freud' :: a -> a
--     freud' x = x
--  b) freud' :: Int -> Int
-- This is fine.
freud' :: Int -> Int
freud' x = x

-- 7.
--  a) myX = 1 :: Int
--     sigmund :: Int -> Int
--     sigmund x = myX
--  b) sigmund :: a -> a
-- No, since we can't match `a` to `Int`

-- 8.
--  a) myX = 1 :: Int
--     sigmund' :: Int -> Int
--     sigmund' x = myX
--  b) sigmund' :: Num a => a -> a
-- This is not fine since we can't guarantee that Num => a will be Int. I.e., under
-- (b) `sigmund' 1.2` doesn't type check, since it must be type signature return 
-- something of type Fractional, but myX :: Int 

-- 9. a) You’ll need to import sort from Data.List:
--      jung :: Ord a => [a] -> a
--      jung xs = head (sort xs)
--  b) jung :: [Int] -> Int
-- THis should be fine, since Int derives Ord
jung :: [Int] -> Int
jung xs = head (sort xs)

-- 10.
--  a) young :: [Char] -> Char
--     young xs = head (sort xs)
--  b) young :: Ord a => [a] -> a
-- This should be fine, since we only need `Ord` to get `sort`
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- 11.
--  a) mySort :: [Char] -> [Char]
--     mySort = sort
--     signifier :: [Char] -> Char
--     signifier xs = head (mySort xs)
--  b) signifier :: Ord a => [a] -> a
-- Won't work, since `mySort` requires that its inputs are of type [Char]. So what
-- would `signifier [1, 2, 3]` do?