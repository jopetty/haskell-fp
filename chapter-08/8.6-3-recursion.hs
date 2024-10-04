-- 8.6-recursion.hs

module Recursion where

-- 1. Write out the steps for reducing dividedBy 15 2 to its final answer according to 
-- the Haskell code.

dividedBy :: Integral a => a -> a -> (a, a) 
dividedBy num denom = go num denom 0
 where go n d count
        | n < d = (count, n)
        | otherwise =
            go (n - d) d (count + 1)

-- 15 divided by 2 ==
--  15 - 2, 13  (subtracted 1 time)
--  13 - 2, 11  (subtracted 2 times)
--  11 - 2, 9   (subtracted 3 times)
--  9 - 2,  7   (subtracted 4 times)
--  7 - 2,  5   (subtracted 5 times)
--  5 - 2,  3   (subtracted 6 times)
--  3 - 2,  1   (subtracted 7 times)
--  1 < 2, so break here returning (7, 1)

-- 2. Write a function that recursively sums all numbers from 1 to n, n being the 
-- argument. So if n is 5, you’d add 1+2+3+4+5 to get 15. The type should be
-- (Eq a, Num a) => a -> a.

triangleSum :: (Eq a, Num a) => a -> a
triangleSum 1 = 1
triangleSum n = n + triangleSum (n - 1)

-- 3. Write a function that multiplies two integral numbers using
--    recursive summation. The type should be
--     `(Integral a) => a -> a -> a

recProd :: (Integral a) => a -> a -> a
recProd x y = go x y 0
 where go x y carry
        | x == 0 = carry
        | x < 0 = go (x+1) y (carry - y)
        | x > 0 = go (x-1) y (carry + y)