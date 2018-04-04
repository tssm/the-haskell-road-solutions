{-# LANGUAGE UnicodeSyntax #-}

import Data.List
import Test.QuickCheck

-- Write a function count to counting the number of occurrences of a character in a string.

count ∷ Char → String → Int
count _ [] = 0
count c (x : xs) = (if x == c then 1 else 0) + (count c xs)

-- Properties

propEqualToElemIndices ∷ Char → String → Bool
propEqualToElemIndices c xs = count c xs == length (elemIndices c xs)

check ∷ IO ()
check = quickCheck propEqualToElemIndices
