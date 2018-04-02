{-# LANGUAGE UnicodeSyntax #-}

import Test.QuickCheck

-- Define a function that gives the maximum of a list of integers. Use the predefined function max.

maxInt ∷ [Int] → Int
maxInt [] = error "empty list"
maxInt [x] = x
maxInt (x : xs) = max x (maxInt xs)

-- Properties

propResultIsBiggest ∷ NonEmptyList Int → Bool
propResultIsBiggest (NonEmpty xs) = all (<= maxInt xs) xs

propResultIsElement ∷ NonEmptyList Int → Bool
propResultIsElement (NonEmpty xs) = x `elem` xs where x = maxInt xs

check ∷ IO ()
check = do
	quickCheck propResultIsBiggest
	quickCheck propResultIsElement
