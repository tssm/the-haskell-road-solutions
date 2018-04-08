{-# LANGUAGE UnicodeSyntax #-}

-- Use map to write a function lengths that takes a list of lists and returns a list of the corresponding list lengths.

lengths ∷ [[a]] → [Int]
lengths = map length

-- Use map to write a function sumLengths that takes a list of lists and returns the sum of their lengths.

sumLengths ∷ [[a]] → Int
sumLengths xs = sum (lengths xs)
