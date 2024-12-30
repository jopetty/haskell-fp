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

-- Vigenère (Chapter 11)

vigenere :: String -> String -> String
vigenere key text
    | text == "" = ""
    | key == "" = text
    | otherwise = zipWith caesarChar keys text
        where keys = [key !! (i `mod` length key) | i <- [0..length text -1]]
              caesarChar k t = head (caesar' (ord k - ord 'A') [t])
