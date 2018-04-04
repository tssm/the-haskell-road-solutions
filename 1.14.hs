{-# LANGUAGE UnicodeSyntax #-}

import Test.QuickCheck

-- Write a function blowup that converts a string a₁a₂a₃... to a₁a₂a₂a₃a₃a₃...

blowup ∷ String → String
blowup []       = []
blowup (x : xs) = process (x : xs) where
	process = helper 1 where
		helper ∷ Int → String → String
		helper _ []       = []
		helper n (x : xs) = replicateChar x n ++ helper (n + 1) xs
	replicateChar c times = helper 1 [] where
		helper counter partialResult
			| counter == times = c : partialResult
			| otherwise        = helper (counter + 1) (c : partialResult)

-- Properties

propLastCharAmount ∷ String → Bool
propLastCharAmount str = all (== lastElement) repeatedLastElement where
	resultString = blowup str
	origilaListLength = length str
	lastElement = str !! (origilaListLength - 1)
	previousElements = (length resultString) - origilaListLength
	repeatedLastElement = drop previousElements resultString

propNewLenght ∷ String → Bool
propNewLenght str = length (blowup str) == sum [1..(length str)]

check ∷ IO ()
check = do
	quickCheck propLastCharAmount
	quickCheck propNewLenght
