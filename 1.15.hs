{-# LANGUAGE UnicodeSyntax #-}

import Data.List
import Test.QuickCheck

-- Write a function srtString ∷ [String] → [String] that sorts a list of strings in alphabeticall order.

mnmStr ∷ [String] → String
mnmStr []       = error "empty list"
mnmStr [x]      = x
mnmStr (x : xs) = min x (mnmStr xs)

removeFst ∷ String → [String] → [String]
removeFst _ [] = []
removeFst m (x : xs)
	| m == x    = xs
	| otherwise = x : removeFst m xs

srtStr ∷ [String] → [String]
srtStr [] = []
srtStr xs = let m = mnmStr xs in m : srtStr (removeFst m xs)

-- Properties

propIdempotent ∷ [String] → Bool
propIdempotent xs = srtStr xs == srtStr (srtStr xs)

propPermutation ∷ [String] → Bool
-- With help from https://codereview.stackexchange.com/questions/191353/quickcheck-properties-for-function-that-sorts-a-list-of-strings
propPermutation xs = isPermutation xs (srtStr xs) where
	isPermutation ∷ Eq a ⇒ [a] → [a] → Bool
	isPermutation xs ys = isSubBag xs ys && isSubBag ys xs where
		isSubBag ∷ Eq a ⇒ [a] → [a] → Bool
		isSubBag []       _  = True
		isSubBag (x : xs) ys = x `elem` ys && isSubBag xs (delete x ys)

propSorted ∷ [String] → Bool
propSorted xs = isSorted (srtStr xs) where
	isSorted ∷ [String] → Bool
	isSorted (x : y : xs) = x <= y && isSorted (y : xs)
	isSorted _            = True

check ∷ IO ()
check = do
	quickCheck propIdempotent
	quickCheck propPermutation
	quickCheck propSorted
