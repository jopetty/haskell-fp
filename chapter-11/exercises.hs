{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# HLINT ignore "Use concat" #-}
{-# HLINT ignore "Use infix" #-}

module Exercises where

import Data.Int
import Data.Char
import Data.List (intersperse)


-- data Bool   = False   | True
--  [1]  [2] [3]   [4] [5]  [6]
-- 
-- [1]: keyword for data declaration
-- [2]: nullary Type Constructor
-- [3]: divides type constructor from data constructors
-- [4]: nullary Data Constructor
-- [5]: sum-type disjunction
-- [6] nullary Data Constructor

-- data [] a   = [ ] | a : [a]
--     [ 7 ]     [8]  [   9  ] 
-- 
-- [7]: Unary Type Constructor
-- [8]: Nullary data constructor for empty list
-- [9]: Binary data constructor with two args: `a` and `[a]`

data Trivial = Trivial'
data UnaryTypeCon a = UnaryValueCon a

-- kinds = types of types
--   represented by *

data PugType = PugData
myPug :: PugType
myPug = PugData :: PugType
-- :t PugData = PugData :: PugType
-- :k PugType = PugType :: *
-- :t myPug = myPug :: PugType

data HuskyType a = HuskyData
myHusky :: HuskyType a
myHusky = HuskyData
myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData
myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData
-- :t HuskyData = HuskyData :: forall {k} (a :: k). HuskyType a
-- :k HuskyType = HuskyType :: k -> *
-- :t myHusky = myHusky :: forall {k} (a :: k). HuskyType a
-- :t myOtherHusky = myOtherHusky :: Num a => HuskyType a
-- :t myOtherOtherHusky = myOtherOtherHusky :: HuskyType [[[[Int]]]]

data DogueDeBordeaux doge = DogueDeBordeaux doge
myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10
-- :t DogueDeBordeaux = DogueDeBordeaux :: doge -> DogueDeBordeaux doge
-- :k DogueDeBordeaux = DogueDeBordeaux :: * -> *
-- :t myDoge = myDoge :: DogueDeBordeaux Int


-- this won't work be bound types don't match
-- badDoge :: DogueDeBordeaux String
-- badDoge = DogueDeBordeaux 10

data Doggies a = 
    Husky a
    | Mastiff a
    deriving (Eq, Show)

-- Exercises: Dog types

-- 1. Is `Doggies` a type constructor or a data constructor?
--    type constructor

-- 2. What is the kind of `Doggies`?
--    Doggies :: * -> *

-- 3. What is the kind of `Doggies String`?
--    Doggies String :: *

-- 4. What is the type of `Husky 10`?
--    Husky 10 :: Num a => Doggies a

-- 5. What is the type of `Husky (10 :: Integer)`
--    Husky (10 :: Integer) :: Doggies Integer

-- 6. What is the type of `Mastiff "Scooby Doo"`?
--    Mastiff "Scooby Doo" :: Doggies String

-- 7. Is `DogueDeBordeaux` a type constructor or a data constructor?
--    Both

-- 8. What is the type of `DogueDeBordeaux`?
--    DogueDeBordeaux :: doge -> DogueDeBordeaux doge

-- 9. What is the type of `DogueDeBordeaux "doggie!"`
--    DogueDeBordeaux "doggie!" :: DogueDeBordeaux String

-- Exercises: Vehicles

data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)
data Manufacturer = 
    Mini
    | Mazda
    | Tata
    deriving (Eq, Show)
data Airline = 
    PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)
data Vehicle = 
    Car Manufacturer Price
    | Plane Airline Size
    deriving (Show, Eq)

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir (Size 747)

-- 1. What is the type of myCar?
--    myCar :: Vehicle

-- Given the following, define the functions:

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3. Now, we’re going to write a function to tell us the manufacturer of a piece of 
--    data:
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

-- 4. Given that we’re returning the Manufacturer, what will happen if you use this on 
--    Plane data?
-- It will return bottom

-- 5. All right. Let’s say you decide to add the size of the plane as an argument to 
--    the Plane constructor. Add that to your datatypes in the appropriate places, and 
--    change your data and functions appropriately.


data Example0 = Example0 deriving (Eq, Show)
data Example1 = Example1 Int deriving (Eq, Show)
data Example2 = Example2 Int String deriving (Eq, Show)

-- Exercises: Cardinality

-- While we haven’t explicitly described the rules for calculating the cardinality of 
-- datatypes yet, you might already have an idea of how to do it for simple datatypes 
-- with nullary constructors. Try not to overthink these exercises—follow your 
-- intuition based on what you know:

-- 1. data PugType = PugData
-- 1

-- 2. data Airline =
--           PapuAir
--           | CatapultsR'Us
--           | TakeYourChancesUnited
-- 3

-- 3. Given what we know about Int8, what’s the cardinality of Int16?
--    2^16 = 65536

-- 4. Use the REPL and maxBound and minBound to examine `Int` and `Integer`. What can 
--    you say about the cardinality of those types?
-- 
--    maxBound :: Int = 9223372036854775807
--    minBound :: Int = -9223372036854775808
--     => Int has cardinality 2^64
-- 
--    maxBound :: Integer = No instance for (Bounded Integer)
--    minBound :: Integer = No instance for (Bounded Integer)
--     => Integer has infinite cardinality (unbounded)

-- 5. Extra credit (impress your friends!): what’s the connection between the 8 in 
--    Int8 and that type’s cardinality of 256?
--    IntN has cardinality 2^N

data Example = MakeExample deriving Show

-- Exercises: For example
-- 1. What is the type of the data constructor MakeExample? What happens when you 
--    request the type of Example?
--    :t MakeExample = MakeExample :: Example
--    :t Example will fail since it doesn't have a type (but it does have a kind = *)

-- 2. What if you try :info on Example in GHCi? Can you determine what type class 
--    instances are defined for the Example type using :info in GHCi?
--    ghci> :info Example
--    type Example :: *
--    data Example = MakeExample
              -- Defined at chapter-11/exercises.hs:187:1
--    instance Show Example -- Defined at chapter-11/exercises.hs:187:37
-- There is a single instance (Show)

-- 3. Try making a new datatype like Example but with a single type argument added to 
--    MakeExample, such as Int. What has changed when you query MakeExample with :type 
--    in GHCi?

data Example' = MakeExample' Int deriving Show
-- :t MakeExample' = MakeExample :: Int -> Example'

data Goats = Goats Int deriving (Eq, Show)

tooManyGoats :: Int -> Bool
tooManyGoats n = n > 42

newtype Goats' = Goats' Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats' :: Goats -> Bool
tooManyGoats' (Goats n) = n > 42


class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany Goats' where
    tooMany (Goats' n) = n > 43

newtype Goats'' = Goats'' Int deriving (Eq, Show, TooMany)


-- Exercises: Logic goats

-- 1. Reusing the TooMany type class, write an instance of the type class for the type 
--    (Int, String). This will require adding a language pragma named 
--    FlexibleInstances if you do not use a newtype— GHC will tell you what to do.

instance TooMany (Int, String) where
    tooMany (n, _) = tooMany n

-- 2. Make another TooMany instance for (Int, Int). Sum the values together under the 
--    assumption that this is a count of goats from two fields.
instance TooMany (Int, Int) where
    tooMany (x, y) = tooMany (x + y)

-- 3. Make another TooMany instance, this time for (Num a, TooMany a) => (a, a). This 
--    can mean whatever you want, such as summing the two numbers together.
instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (a, b) = tooMany (a * b)


-- Exercises: Pity the Bool

-- 1. Given a datatype:
data BigSmall =
    Big Bool
    | Small Bool
    deriving ( Eq, Show)
--    What is its cardinality? Hint: We already know Bool’s cardinality. Show your 
--    work, as demonstrated earlier.
-- It has cardinality 4, since each option for the sum type has cardinality 2

-- 2. Given a datatype:
data NumberOrBool =
    Numba Int8
    | BoolyBool Bool
    deriving ( Eq, Show) 

myNumba = Numba (- 128)

--   What is the cardinality of NumberOrBool? What happens if you try to create a Numba 
--   with a numeric literal larger than 127? And with a numeric literal smaller than 
--   -128?

-- NumberOrBool has cadinality 2 + 2^8 = 258

-- Cardinality 3
data QuantumBool = 
    QuantumTrue
    | QuantumFalse
    | QuantumBoth
    deriving (Eq, Show)

-- Product types (no special syntax, cf `|` for Sum Types)

data TwoQs = 
    MkTwoQs QuantumBool QuantumBool
    deriving (Eq, Show)

-- Alternatively:
-- type TwoQs = (QuantumBool, QuantumBool)


-- Record = Product Types w/ sugar for accessors of fields
data Person =
    MkPerson String Int
    deriving (Eq, Show)

-- sample data
jm = MkPerson "julie" 108
ca = MkPerson "chris" 16

namae :: Person -> String
namae (MkPerson s _) = s

data Person' = 
    Person' {
        name :: String,
        age :: Int
    } deriving (Eq, Show)

-- now `name` and `age` are functions from Person' -> {String, Int}


-- Distributivity
data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = 
    FictionBook Fiction 
    | NonfinctionBook Nonfiction
    deriving Show

type AuthorName = String

-- data Author = Author (AuthorName, BookType) -- product type

-- could rewrite in normal form (sum of products)
data Author' = 
    Fiction' AuthorName
    | Nonfiction' AuthorName
    deriving (Eq, Show)

-- Exercises: How does your garden grow?
-- 1. Given the type:
data FlowerType = 
    Gardenia
    | Daisy 
    | Rose 
    | Lilac
    deriving Show

type Gardener = String

data Garden = 
    Garden Gardener FlowerType
    deriving Show

-- What is the sum of products normal form of Garden?

data Garden' =
    Gardenia' Gardener
    | Daisy' Gardener 
    | Rose' Gardener
    | Lilac' Gardener
    deriving Show


-- (de)constructors

data GuessWhat =
    ChickenButt deriving (Eq, Show)

data Id a =
    MkId a deriving (Eq, Show)

data Product a b =
    Product a b deriving (Eq, Show)

data Sum a b = 
    First a
    | Second b
    deriving (Eq, Show)

data RecordProduct a b =
    RecordProduct {
        pfirst :: a,
        psecond :: b
    } deriving (Eq, Show)

newtype NumCow =
    NumCow Int 
    deriving (Eq, Show)

newtype NumPig =
    NumPig Int
    deriving (Eq, Show)

data Farmhouse =
    Farmhouse NumCow NumPig
    deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

-- Farmhouse and Farmhouse' are the same

newtype NumSheep =
    NumSheep Int
    deriving (Eq, Show)

data BigFarmhouse = 
    BigFarmhouse NumCow NumPig NumSheep
    deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo =
    CowInfo Name Age
    deriving (Eq, Show)

data PigInfo =
    PigInfo Name Age LovesMud
    deriving (Eq, Show)

data SheepInfo =
    SheepInfo Name Age PoundsOfWool
    deriving (Eq, Show)

data Animal = 
    Cow CowInfo
    | Pig PigInfo 
    | Sheep SheepInfo
    deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

data Twitter = 
    Twitter deriving (Eq, Show)

data AskFm = 
    AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter


data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
    Haskell 
    | Agda
    | Idris 
    | PureScript
    deriving (Eq, Show)

data Programmer = 
    Programmer {
        os :: OperatingSystem,
        lang :: ProgLang
    }
    deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer {
    os = Mac,
    lang = Haskell
}

feelingWizardly :: Programmer
feelingWizardly = Programmer {
    lang = Agda,
    os = GnuPlusLinux
}

-- Exercise: Programmers
-- Write a function that generates all possible values of Programmer. Use the provided 
-- lists of inhabitants of OperatingSystem and ProgLang:

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgLang]
allLanguages = 
    [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]


newtype Name' = Name' String deriving Show
newtype Acres = Acres Int deriving Show

-- FarmerType is a sum type
data FarmerType = 
      DairyFarmer
    | WheatFarmer
    | SoybeanFarmer
    deriving Show

-- Farmer is a product type
data Farmer = 
    Farmer Name' Acres FarmerType
    deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec = 
    FarmerRec { 
      name'  :: Name'
    , acres :: Acres
    , farmerType :: FarmerType }
    deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
    case farmerType farmer of
        DairyFarmer -> True
        _           -> False



data Quantum = 
      Yes
    | No
    | Both
    deriving (Eq, Show)

quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

quantSum5 :: Either Quantum Quantum
quantSum5 = Left No

quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both

-- Consider the following function:

convert :: Quantum -> Bool
convert = undefined

-- According to the equality of a -> b and b^a, there should be 2^3 or 8 
-- implementations of this function. Does this hold? Write it out, and prove it for 
-- yourself.

convert1 :: Quantum -> Bool
convert1 c = case c of
    Yes -> True
    No -> True
    Both -> True

convert2 :: Quantum -> Bool
convert2 c = case c of
    Yes -> True
    No -> True
    Both -> False


convert3 :: Quantum -> Bool
convert3 c = case c of
    Yes -> True
    No -> False
    Both -> True

convert4 :: Quantum -> Bool
convert4 c = case c of
    Yes -> False
    No -> True
    Both -> True

convert5 :: Quantum -> Bool
convert5 c = case c of
    Yes -> True
    No -> False
    Both -> False

convert6 :: Quantum -> Bool
convert6 c = case c of
    Yes -> False
    No -> True
    Both -> False

convert7 :: Quantum -> Bool
convert7 c = case c of
    Yes -> False
    No -> False
    Both -> True


convert8 :: Quantum -> Bool
convert8 c = case c of
    Yes -> False
    No -> False
    Both -> False

-- Exercises: The Quad
-- Determine how many unique inhabitants each type has.
-- 

-- 1.
data Quad =
      One
    | Two
    | Three
    | Four
    deriving (Eq, Show)

-- How many different forms can this take?
eQuad :: Either Quad Quad
eQuad = undefined
-- 4+4 = 8

-- 2. 
prodQuad :: (Quad, Quad)
prodQuad = undefined
-- 4*4 = 16

-- 3. 
funcQuad :: Quad -> Quad
funcQuad = undefined
-- 4^4 = 256

-- 4. 
prodTBool :: ( Bool, Bool, Bool)
prodTBool = undefined
-- 2*2*2 = 8

-- 5. 
gTwo :: Bool -> Bool -> Bool
gTwo = undefined
-- (2^2)^2 = 16

-- 6.
fTwo :: Bool -> Quad -> Quad
fTwo = undefined
-- (2^4)^4 = 65,536

-- Higher kinds
data Silly a b c d =
    MkSilly a b c d deriving Show

-- ghci> :k Silly
-- Silly :: * -> * -> * -> * -> *

data BinaryTree a =
      Leaf
    | Node ( BinaryTree a) a (BinaryTree a) 
    deriving ( Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a  = Node (insert' b left) a right
    | b > a  = Node left a (insert' b right) 

-- Write map for BinaryTree
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f t = case t of 
    Leaf -> Leaf
    Node left a right -> Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf ) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay :: IO ()
mapOkay = 
    if mapTree (+1) testTree' == mapExpected
        then print "yup OK!"
        else error "test failed!"

-- Convert binary trees to lists
preorder :: BinaryTree a -> [a]
preorder t = case t of 
    Leaf -> []
    Node left v right -> [v] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder t = case t of
    Leaf -> []
    Node left v right -> preorder left ++ [v] ++ preorder right

postorder :: BinaryTree a -> [a]
postorder t = case t of
    Leaf -> []
    Node left v right -> preorder left ++ preorder right ++ [v]

testTree :: BinaryTree Integer
testTree =
    Node (Node Leaf 1 Leaf)
         2
         (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = 
    if preorder testTree == [2,1,3]
        then putStrLn "Preorder fine!"
        else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder = 
    if inorder testTree == [1,2,3]
        then putStrLn "Inorder fine!"
        else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1,3,2]
        then putStrLn "Postorder fine!"
        else putStrLn "Bad news bears."

main :: IO ()
main = do
    testPreorder 
    testInorder
    testPostorder


-- write foldr for BinaryTree
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f z t = case t of 
    Leaf -> z
    Node left v right -> foldTree f (foldTree f (f v z) left) right


-- Chapter Exercises
-- Multiple Choice

-- 1. Given the following datatype:
data Weekday = 
      Monday
    | Tuesday
    | Wednesday 
    | Thursday
    | Friday
-- Which of the following is true?
-- (a) `Weekday` is a type with five data constructors.

-- 2. With the same datatype definition in mind, what is the type of the following 
-- function, `f` ?

-- f Friday = "Miller Time"
-- (c) f :: Weekday -> String

-- 3. Types defined with the data keyword:
--    (b) Must begin with a capital letter.

-- 4. The function `g xs = xs !! (length xs - 1)`:
--    (c)   Returns the final element of `xs`.
--    I don't think it's (a) since it's not recursive, though it very well may not 
--    terminate

-- Ciphers
-- see chapter-09/ciphers.hs

-- Use as-patterns to implement the following functions:

-- 1. This should return `True` if (and only if) all the values in the first list 
--    appear in the second list, though they need not be contiguous:
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf (x:xs) y@(_:ys) = elem x y && isSubseqOf xs ys

-- 2. Split a sentence into words, then tuple each one with its capitalized form:
capitalizeWords :: String -> [(String, String)]
capitalizeWords t = map (\w@(f:rest) -> (w, toUpper f : rest)) (words t)

-- Language exercises

-- 1. Write a function that capitalizes a word:
capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (f:rest) = toUpper f : rest

-- 2. Write a function that capitalizes sentences in a paragraph. Recognize when a 
--    new sentence has begun by checking for periods. Reuse the capitalizeWord function:
capitalizeParagraph :: String -> String
capitalizeParagraph t = restorePeriod t $ joinWithSpace $ map (capitalizeWord . stripSpace) (splitOn '.' t)

-- See chapter-09/exercises.hs for original implementation
splitOn :: Char -> String -> [String]
splitOn c s
    | s == "" = []
    | otherwise = 
        let first = takeWhile (/= c) s
            rest = dropWhile (== c) (dropWhile (/= c) s)
        in first : splitOn c rest

stripSpace :: String -> String
stripSpace s
    | s == "" = ""
    | head s == ' ' = tail s
    | otherwise = s

joinWithSpace :: [String] -> String
joinWithSpace xs = foldr (++) "" (intersperse ". " xs)

restorePeriod :: String -> String -> String
restorePeriod original new
    | original == "" = new
    | otherwise = if original !! (length original - 1) == '.'
        then new ++ "."
        else new

-- Phone Exercise

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

data DaPhone = 
    DaPhone [(Char, String)] 
    deriving (Eq, Show)

defaultLayout :: DaPhone
defaultLayout = 
    DaPhone [
        ('1', ""),
        ('2', "ABC"),
        ('3', "DEF"),
        ('4', "GHI"),
        ('5', "JKL"),
        ('6', "MNO"),
        ('7', "PQRS"),
        ('8', "TUV"),
        ('9', "WXYZ"),
        ('*', "^"),
        ('0', "+ _"),
        ('#', ".,")
    ]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone []) _ = [('?', 0)]
reverseTaps (DaPhone ((digit, letters):ls)) c = 
    if elem (toUpper c) letters || c == digit
        then charToTaps digit c letters
        else reverseTaps (DaPhone ls) c

charToTaps :: Digit -> Char -> String -> [(Digit, Presses)]
charToTaps d t letters
    | d == t = [(d, length letters + 1)]
    | otherwise =
        if elem t letters 
            then 
                if elem t "^+ _.," 
                    then [(d, indexOf t letters)] 
                    else [('*', 1), (d, indexOf t letters)]
            else [(d, indexOf (toUpper t) letters)]
    where indexOf target (l:ls) = if target == l then 1 else 1 + indexOf target ls

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead _ [] = []
cellPhonesDead l (x:xs) = reverseTaps l x ++ cellPhonesDead l xs

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps xs = sum (map snd xs)