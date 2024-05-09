-- typeKwanDo.hs

module TypeKwanDo where

-- 1. 
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y

-- 2. Hint: use some arithmetic operation to combine values of type b. Pick one:
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f z x = (f x) + fromIntegral z