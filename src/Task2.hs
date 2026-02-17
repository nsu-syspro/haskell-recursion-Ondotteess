{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- The above pragma enables all warnings
-- (except for unused imports from Task1)

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (filter, foldl, foldr, head, init, last, length, map, read, reverse, show, sum, tail)

-- You can reuse already implemented functions from Task1
-- by listing them in this import clause
-- NOTE: only listed functions are imported, everything else remains hidden
import Task1 (map, reverse, sum)

-----------------------------------
--
-- Computes check digit number for given abstract characters using Luhn algorithm mod N
-- and given mapping function
--
-- Usage example:
--
-- >>> luhnModN 10 id [3,4,5,6]
-- 1

luhnModN :: Int -> (a -> Int) -> [a] -> Int
luhnModN n f xs =
  let digits = map f xs
      s = sum (map (normalizeModN n) (doubleEveryOther (reverse digits)))
      r = s `mod` n
  in (n - r) `mod` n

-----------------------------------
--
-- Computes decimal check digit for given digits using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> luhnDec [3,4,5,6]
-- 1

luhnDec :: [Int] -> Int
luhnDec = luhnModN 10 id

-----------------------------------
--
-- Computes hexadecimal check digit number for given digits using Luhn algorithm mod 16
--
-- Usage example:
--
-- >>> luhnHex "123abc"
-- 15

luhnHex :: [Char] -> Int
luhnHex = luhnModN 16 digitToInt

-----------------------------------
--
-- Converts given hexadecimal digit to its ordinal number between 0 and 15
--
-- Usage example:
--
-- >>> map digitToInt ['0'..'9']
-- [0,1,2,3,4,5,6,7,8,9]
-- >>> map digitToInt ['a'..'f']
-- [10,11,12,13,14,15]
-- >>> map digitToInt ['A'..'F']
-- [10,11,12,13,14,15]

digitToInt :: Char -> Int
digitToInt c
  | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' = 10 + (fromEnum c - fromEnum 'a')
  | c >= 'A' && c <= 'F' = 10 + (fromEnum c - fromEnum 'A')
  | otherwise            = 0

-----------------------------------
--
-- Checks whether the last decimal digit is a valid check digit
-- for the rest of the given number using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> validateDec 3456
-- False
-- >>> validateDec 34561
-- True
-- >>> validateDec 34562
-- False

validateDec :: Integer -> Bool
validateDec n =
  let ds = toDigitsDec n
  in case ds of
       [] -> False
       _  -> luhnDec (initList ds) == lastElem ds

-----------------------------------
--
-- Checks whether the last hexadecimal digit is a valid check digit
-- for the rest of the given number using Luhn algorithm mod 16
--
-- Usage example:
--
-- >>> validateHex "123abc"
-- False
-- >>> validateHex "123abcf"
-- True
-- >>> validateHex "123abc0"
-- False

validateHex :: [Char] -> Bool
validateHex xs =
  case xs of
    [] -> False
    _  -> luhnHex (initList xs) == digitToInt (lastElem xs)


-----------------------------------


doubleEveryOther :: [Int] -> [Int]
doubleEveryOther = go True
  where
    go _ [] = []
    go True (x:xs)  = (2 * x) : go False xs
    go False (x:xs) = x       : go True  xs

normalizeModN :: Int -> Int -> Int
normalizeModN n x
  | x >= n    = x - (n - 1)
  | otherwise = x

toDigitsDec :: Integer -> [Int]
toDigitsDec k
  | k <= 0    = []
  | otherwise = toDigitsDec (k `div` 10) ++ [fromInteger (k `mod` 10)]

lastElem :: [a] -> a
lastElem [x]    = x
lastElem (_:xs) = lastElem xs
lastElem []     = error "empty list"

initList :: [a] -> [a]
initList [_]    = []
initList (x:xs) = x : initList xs
initList []     = []
