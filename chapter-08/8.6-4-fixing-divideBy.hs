-- 8.6-4-fixing-divideBy.hs

module FixedDivideBy where

data DividedResult = 
    Result Integer
    | DividedByZero
    deriving Show


dividedBy' :: Integral a => a -> a -> DividedResult
dividedBy' num denom = go num denom 0
    where go num denom carry
            | denom == 0 = DividedByZero
            | denom < 0 = case dividedBy' num (-denom) of
                DividedByZero -> DividedByZero
                Result r -> Result (-r)
            | num < 0 = case dividedBy' (-num) denom of
                DividedByZero -> DividedByZero
                Result r -> Result (-r)
            | num < denom = Result carry
            | otherwise = go (num - denom) denom (carry + 1)