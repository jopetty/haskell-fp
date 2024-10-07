{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Exercises where

import Data.Int


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