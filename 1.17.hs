{-# LANGUAGE UnicodeSyntax #-}

import Test.QuickCheck

-- Write a function substring ∷ String → String -> Bool that checks whether str1 is a substring of str2.

substring ∷ String → String → Bool
substring (_ : _) []        = False
substring []      []        = True
-- If ys equals y:ys' and xs is substring of ys', xs is substring of ys:
substring xs      (y : ys') = prefix xs (y : ys') || substring xs ys' where
	prefix ∷ String → String → Bool
	-- If xs is a prefix of ys, xs is substring of ys:
	prefix (_ : _)  []       = False
	prefix []       _        = True
	prefix (x : xs) (y : ys) = x == y && prefix xs ys

-- Properties

propSubstr ∷ NonEmptyList Char → NonNegative Int → NonNegative Int → Property
propSubstr (NonEmpty str) (NonNegative x) (NonNegative y) =
	x < y && y < length str ==> substring (drop x . take y $ str) str

check ∷ IO ()
check = quickCheck propSubstr
