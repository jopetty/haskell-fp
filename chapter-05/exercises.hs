-- exercises.hs

module Exercises where

-- 1. Given the type `a -> a`, which is the type for `id`, attempt to make a function that terminates successfully but that does something other than return the same value. This is impossible, but you should try it anyway.

-- fakeId :: a -> a
-- fakeId x = do
--     putStrLn "I'm a fake id"
--     x

-- 2. We can get a more comfortable appreciation of parametricity by looking at `a -> a -> a`. This hypothetical function `a -> a -> a` has only two implementations. Write both possible versions of `a -> a -> a`. After doing so, try to violate the constraints of parametrically polymorphic values we outlined above.

doubleTrouble1 :: a -> a -> a
doubleTrouble1 x y = x

doubleTrouble2 :: a -> a -> a
doubleTrouble2 x y = y


-- 3. Implement `a -> b -> b`. How many implementations can it have? Does its behavior change when the types of `a` and `b` change?

abb1 :: a -> b -> b
abb1 x y = y

-- I don't think there are any other possible implementations of this function. Since we must return something of type `b` the only value we can pick is `y :: b`. The functions behavior does not change when the types of `a` and `b` change, but it would change if we change the type signature to constrain `b`'s type. For instance, if we constrain `b` to be of type `Num b => b, then we could do something like the following:

abb2 :: Num b => a -> b -> b
abb2 x y = y + 1


-- SECTION: Apply Yourself

myConcat x = x ++ " yo"
-- has the type signature: myConcat :: [Char] -> [Char]

myMult x = (x / 3) * 5
-- has the type signature: myMult :: Fractional a => a -> a

myTake x = take x "hey you"
-- has the type signature: myTake :: Int -> [Char]

myCom x = x > (length [1..10])
-- has the type signature: myCom :: Int -> Bool

myAlph x = x < 'z' 
-- has the type signature: myAlph :: Char -> Bool