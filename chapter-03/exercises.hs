-- exercises.hs
module Exercises where

dropFirst :: String -> String
dropFirst s = drop 1 s

dropLast :: String -> String
dropLast s = take (length s - 1) s

charAt4 :: String -> Char
charAt4 s = s !! 4

after8 :: String -> String
after8 s = drop 9 s

thirdLetter :: String -> Char
thirdLetter s = s !! 2

letterIndex :: Int -> Char
letterIndex i = "Curry is awesome!" !! i

curryAwesome :: String
curryAwesome = "Curry is awesome"

rvrs :: String
rvrs = drop 9 curryAwesome ++ " " ++ take 2 (drop 6 curryAwesome) ++ " " ++ take 5 curryAwesome