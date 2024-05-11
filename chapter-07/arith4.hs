-- arith4.hs

module Arith4 where

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

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- 5. Next, write a point-free version of roundTrip. (n.b., this refers to the function 
--    definition, not to its application in main.)
roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

-- 6. We will continue to use the code in module Arith4 for this exercise, as well.

--    When we apply show to a value such as (1 :: Int), the a that implements Show is 
--    type Int, so GHC will use the Int instance of the Show type class to stringify /
--    our Int value 1.

--    However, read expects a String argument in order to return an a. The String 
--    argument that is the first argument to read tells the function nothing about what 
--    type the de-stringified result should be. In the type signature roundTrip 
--    currently has, it knows, because the type variables are the same, so the type 
--    that is the input to show has to be the same type as the output of read.

--    Your task now is to change the type of roundTrip in Arith4 to (Show a, Read b) => 
--    a -> b. How might we tell GHC which instance of Read to dispatch against the 
--    String? Make the expression print (roundTrip 4) work. You will only need the has 
--    the type syntax of :: and parentheses for scoping.

roundTrip' :: (Show a, Read b) => a -> b
roundTrip' = read . show

main = do
    print (roundTrip 4)
    print (roundTripPF 4)
    print (roundTrip' 4 :: Int)
    print (id 4)