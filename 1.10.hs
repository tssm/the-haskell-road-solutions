{-# LANGUAGE UnicodeSyntax #-}

import Data.List
import Test.QuickCheck

-- Define a function removeFst that removes the first occurrence of an integer m from a list of integers. If m does not occur in the list, the list remains unchanged.

removeFst ∷ Int → [Int] → [Int]
removeFst _ [] = []
removeFst m (x : xs)
	| m == x    = xs
	| otherwise = x : removeFst m xs

-- Properties

propIndex ∷ Int → [Int] → Bool
propIndex m ms = case elemIndex m ms of
	Just n → case elemIndex m $ removeFst m ms of
		Just m  → n <= m
		Nothing → True
	Nothing → True

propLength ∷ Int → [Int] → Bool
propLength m ms = x == y || x + 1 == y where
	x = length $ removeFst m ms
	y = length ms

check ∷ IO ()
check = do
	quickCheck propIndex
	quickCheck propLength
