-- Exercises: Understanding folds

-- 1. `foldr (*) 1 [1..5]` will return the same result as:
--   (1 * (2 * (3 * (4 * (5 * 1))))) = 120
--  = (b) and (c), while (a) is not well-formed
-- `foldl (*) 1 [1..5]` = 
--   (((((1 * 1) * 2) * 3 ) * 4 ) * 5) = 120

-- 2. Write out the evaluation steps for `foldl (flip (*)) 1 [1..3]`
-- (((1 `flip (*)` 1) `flip (*)` 2) `flip (*)` 3)
-- (((1 * 1) `flip (*)` 2) `flip (*)` 3)
-- ((1 `flip (*)` 2) `flip (*)` 3)
-- ((2 * 1) `flip (*)` 3)
-- (2 `flip (*)` 3)
-- (3 * 2)
-- 6

-- 3. One difference between `foldr` and `foldl` is:
--    (c) `foldr`, but not `foldl`, associates to the right

-- 4. Folds are catamoprphisms, which means they are generally used to
--    (a) Reduce structure

-- 5. The following are simple folds very similar to what you've already seen, but
--    each has at least one error. Please fix and test them in your REPL:
--    (a) `foldr (++) ["woot", "WOOT", "woot"]`
--        This doesn't contain a zero/unit value, which for strings should be `""`:
-- 
--        `foldr (++) "" ["woot", "WOOT", "woot"]`
--                    ^^

--    (b) `foldr max [] "fear is the little death"`
--        This has the wrong unit value; the goal of the function seems to be to
--        return the character with the highest value, so the initial value should
--        be something like 'a' or ' ':
-- 
--        `foldr max ' ' "fear is the little death"`

--    (c) `foldr and True [ False, True]`
--        Expression uses the wrong conjunction operator: should be 
--        `foldr (&&) True [False, True]`

--    (d) `foldr (||) True [ False, True]`
--        The unit value is wrong; (||) should take `False` as the initial value,
--        since otherwise the expression will always evaluate to true:
--        `foldr (||) False [False, True]`

--    (e) `foldl ((++) . show) "" [1..5]`
--        The first argument to `foldl` must be a function of type `b -> a -> b`, where
--        `b` is the accumulator type and `a` is the sequence type; since we're turning
--        a list of integers into strings, this argument should be a function of type
--        `String -> Int -> String`. However, the argument actually passed to `foldl`,
--        `((++) . show)` has type `a -> String -> String`. Thus, type mismatch. We
--        can fix by using `foldr` instead:
--        `foldr ((++) . show) "" [1..5]` = "12345"

--    (f) `foldr const 'a' [1..5]`
--        Opposite problem as before; `const` has type `a -> b -> a`, while `foldr`
--        expects its first argument to be of type `b -> a -> a`; we can fix this by
--        switching to `foldl`:
--        `foldl const 'a' [1..5]` = 'a'

--    (g) `foldr const 0 "tacos"`
--        Assuming the goal is to get out `0`, the type of `const` is wrong for `foldr`,
--        and we should use `foldl` instead:
--        `foldl const 0 "tacos"` = 0

--    (h) `foldl (flip const) 0 "burritos"`
--        Types. Go back to `foldr`:
--        `foldr (flip const) 0 "burritos"` = 0

--    (i) `foldl (flip const) 'z' [1..5]`
--         same deal; either get rid of `flip` or use `foldr`
--         `foldr (flip const) 'z' [1..5]` = 'z'
--         `foldl const 'z' [1..5]` = 'z'

-- Exercises: Database processing
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use or" #-}
{-# HLINT ignore "Use elem" #-}
{-# HLINT ignore "Use concatMap" #-}
{-# HLINT ignore "Use concat" #-}

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
    [ 
        DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
        DbNumber 9001,
        DbString "Hello, world!",
        DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]


-- 1. Write a function that filters for DbDate values and returns a list of the UTCTime 
--    values inside them:
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr concatDates []
    where concatDates (DbDate t) ts = t : ts
          concatDates _ ts = ts

-- 2. Write a function that filters for DbNumber values and returns a list of the 
--    Integer values inside them:
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr concatNumbers []
    where concatNumbers (DbNumber t) ts = t : ts
          concatNumbers _ ts = ts

-- 3. Write a function that gets the most recent date:
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4. Write a function that sums all of the DbNumber values:
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5. Write a function that gets the average of the DbNumber values:
-- avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sum dbNums) / fromIntegral (length dbNums)
    where dbNums = filterDbNumber db

-- Scans exercises

fibs = 1 : scanl (+) 1 fibs
fibsN n = fibs !! n

-- 1. Modify your fibs function to only return the first 20 Fibonacci numbers.
fibs20 = take 20 (1 : scanl (+) 1 fibs)

-- 2. Modify fibs to return the Fibonacci numbers that are less than 100.
fibsSmall = takeWhile (< 100) fibs

-- 3. Try to write the factorial function from Chapter 8 as a scan. You’ll want scanl 
--    again, and your start value will be 1. Warning: this will also generate an 
--    infinite list, so you may want to pass it through a take function or similar.

facs = scanl (*) 1 [1..]

facsN n = facs !! n

-- Chapter Exercises

-- Warm-up and review

-- 1. Given the following sets of consonants and vowels:
stops = "pbtdkg"
vowels = "aeiou"

--    (a) Write a function that takes inputs from stops and vowels and makes 3-tuples 
--        of all possible stop-vowel-stop combinations. These will not all correspond 
--        to real words in English, although the stop-vowel-stop pattern is common 
--        enough that many of them will.
closedOnsetSylls :: [(Char,Char,Char)]
closedOnsetSylls = [(o,v,c) | o <- stops, v <- vowels, c <- stops]

--    (b) Modify that function so that it only returns the combinations that begin with 
--        a p.
closedPSylls :: [(Char,Char,Char)]
closedPSylls = [('p',v,c) | v <- vowels, c <- stops]

--    (c) Now set up lists of nouns and verbs (instead of stops and vowels), and modify 
--        the function to make tuples represent- ing possible noun-verb-noun sentences.
nouns = ["bats", "tigers", "lions", "fish", "bears", "birds"]
verbs = ["see", "hear", "fear", "like", "eat"]

xyxTuples :: [a] -> [a] -> [(a,a,a)]
xyxTuples x y = [(f,s,t) | f <- x, s <-y, t <- x]


-- 2. What does the following mystery function do? What is its type? Try to get a good 
--    sense of what it does before you test it in the REPL to verify it:
seekritFunc x =
    div (sum (map length (words x)))
        (length (words x))

-- This function returns the average number of characters per word (eg, total non-space 
-- characters in sentence divided by the number of space-separated words)

-- 3. We’d really like the answer to be more precise. Can you rewrite that using 
-- fractional division?
seekritFunc' :: Fractional a => String -> a
seekritFunc' x = 
    fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

-- Rewriting functions using folds, ideally point-free

-- 1. myOr
-- fold & point-free
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2. myAny
-- fold, not point-free
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||).f) False

-- fold & PF
myAny' :: (a -> Bool) -> [a] -> Bool
myAny' = flip foldr False . ((||) .)

-- 3. myElem
-- fold
myElem :: Eq a => a -> [a] -> Bool
myElem = flip foldr False . ((||) . ) . (==)

-- any
myElem' :: Eq a => a -> [a] -> Bool
myElem' = any . (==)

-- 4. myReverse
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5. myMap
myMap :: (a -> b) -> [a] -> [b]
-- myMap f = foldr (flip (\acc c -> f c : acc)) [] -- original
-- myMap f = foldr ((:) . f) [] -- remove the lambda
myMap = flip foldr [] . ((:) .) -- point free

-- 6. myFilter w/ foldr
myFilter :: (a -> Bool) -> [a] -> [a]
-- myFilter pred ls = (foldr ((++) . (\a -> if pred a then [a] else [])) [] ) ls -- original
-- myFilter pred = foldr ((++) . (\a -> if pred a then [a] else [])) [] -- eta reduce
-- myFilter pred = foldr ((++) . (\a -> [a | pred a])) [] -- list comprehension
-- myFilter pred = flip foldr [] ((++) . (\a -> [a | pred a])) -- flip
myFilter = flip foldr [] . ((++) .) . (\f a -> [a | f a]) -- point free

-- 7. squish
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8. squishMap
squishMap :: (a -> [b]) -> [a] -> [b]
-- squishMap f ls = squish $ map f ls -- original
-- squishMap f ls = foldr ((++) . f) [] ls -- write out squish def
-- squishMap f = foldr ((++) . f) [] -- eta reduce
-- squishMap f = flip foldr [] ((++) . f) -- flip args to prepare for pf
squishMap = flip foldr [] . ((++) .) -- point free

-- 9. squishAgain
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10. myMaximumBy
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a

-- original
-- myMaximumBy pred ls = head $ foldr (\a b -> 
--     case b of
--         [] -> [a]
--         (b:_) -> if pred a b == GT then [a] else [b]
--     ) [] ls

-- remove last argument
myMaximumBy pred = head . foldr (\a b ->
    case b of
        [] -> [a]
        (b:_) -> if pred a b == GT then [a] else [b]
    ) []

-- myMaximumBy pred = head . flip foldr [] (\a b ->
--     case b of 
--         [] -> [a]
--         (b:_) -> if pred a b == GT then [a] else [b]
--     )

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy pred = head . foldr (\a b ->
    case b of
        [] -> [a]
        (b:_) -> if pred a b == LT then [a] else [b]
    ) []