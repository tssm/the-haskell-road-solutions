{-# LANGUAGE UnicodeSyntax #-}

-- Write Haskell definitions of contradictions tests for frofositional functions with one, two and three variables.

test1 ∷ (Bool → Bool) → Bool
test1 f = not (f True) && not (f False)

test2 ∷ (Bool → Bool → Bool) → Bool
test2 f = and [not (f p q) | p ← [True, False], q ← [True, False]]

test3 ∷ (Bool → Bool → Bool → Bool) → Bool
test3 f = and [not (f p q r) |
	p ← [True, False],
	q ← [True, False],
	r ← [True, False]]
