{-# LANGUAGE UnicodeSyntax #-}

lequiv ∷ (Bool → Bool) → (Bool → Bool) → Bool
lequiv p q = (p True == q True) && (p False == q False)

infix 1 ==>
(==>) ∷ Bool → Bool → Bool
x ==> y = not x || y

-- ¬⟙ ≡ ⟘
check1a ∷ Bool
check1a = not True == False

-- ¬⟘ ≡ ⟙
check1b ∷ Bool
check1b = not False == True

-- p ⇒ ⟘ ≡ ¬p
check2 ∷ Bool
check2 = lequiv (\ p → p ==> False) (\ p → not p)

-- p ∨ ⟙ ≡ ⟙
check3a ∷ Bool
check3a = lequiv (\ p → p || True) (const True)

-- p ∧ ⟘ ≡ ⟘
check3b ∷ Bool
check3b = lequiv (\ p → p && False) (const False)

-- p ∨ ⟘ ≡ p
check4a ∷ Bool
check4a = lequiv (\ p → p || False) id

-- p ∧ ⟙ ≡ p
check4b ∷ Bool
check4b = lequiv (\ p → p && True) id

-- p ∨ ¬p ≡ ⟙
check5a ∷ Bool
check5a = lequiv (\ p → p || not p) (const True)

-- p ∧ ¬p ≡ ⟘
check5b ∷ Bool
check5b = lequiv (\ p → p && not p) (const False)
