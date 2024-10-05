module Ciper where

import Data.Char

alphSize = ord 'z' - ord 'A'

alphMod :: Int -> Int
alphMod n = mod n alphSize

caesar' :: Int -> String -> String
caesar' n = map (chr . (+ord 'A') . alphMod . (+n) . subtract (ord 'A') . ord)

unCaesar' :: Int -> String -> String
unCaesar' n = caesar' (alphSize - n)

-- Set the default interface to use a shift of 15 (ref. Ides of March)
caesar :: String -> String
caesar = caesar' 15

unCaesar :: String -> String
unCaesar = unCaesar' 15
