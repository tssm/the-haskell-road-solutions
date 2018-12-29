{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

class TF p where
	valid ∷ p → Bool
	lequiv ∷ p → p -> Bool

instance TF Bool where
	valid = id
	lequiv f g = f == g

instance TF p ⇒ TF (Bool → p) where
	valid f = valid (f True) && valid (f False)
	lequiv f g =
		(f True) `lequiv` (g True) &&
		(f False) `lequiv` (g False)

infix 1 ==>
(==>) ∷ Bool → Bool → Bool
x ==> y = not x || y

test1 ∷ Bool
test1 = lequiv (\ p q → not p ==> q) (\ p q -> p ==> not q)

test2 ∷ Bool
test2 = lequiv (\ p q → not p ==> q) (\ p q -> q ==> not p)

test3 ∷ Bool
test3 = lequiv (\ p q → not p ==> q) (\ p q -> not q ==> p)

test4 ∷ Bool
test4 = lequiv (\ p q r → p ==> (q ==> r)) (\ p q r -> q ==> (p ==> r))

test5 ∷ Bool
test5 = lequiv (\ p q r → p ==> (q ==> r)) (\ p q r -> (p ==> q) ==> r)

test6 ∷ Bool
test6 = lequiv (\ p q → (p ==> q) ==> p) (\ p _ -> p)

test7 ∷ Bool
test7 = lequiv (\ p q r → p || q ==> r) (\ p q r -> (p ==> r) && (q ==> r))
