{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P a) (P b) = (stripZeros a) (stripZeros b) -- P [1,0] and P [1] are equal - 1 + 0x
        where
            polyEqual :: (Num a, Eq a) => [a] -> [a] -> Bool
            polyEqual [] [] = True
            polyEqual [] nums = False
            polyEqual nums [] = False
            polyEqual (x:xs) (y:ys) = x == y && polyEqual xs ys

            stripZeros :: (Eq a, Num a) => [a] -> [a]
            stripZeros = reverse . dropWhile (== 0) . reverse
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P []) = "0"
    show (P coeffs)
        | null terms = "0"
        | otherwise  = intercalate " + " terms
      where
        -- Strip trailing zeroes
        clean = reverse (dropWhile (== 0) (reverse coeffs))

        -- Pair each coefficient with its power
        indexed = zip clean [0..]

        -- Format terms (filter out zero coefficients)
        terms = map (uncurry showTerm) (filter (\(c, _) -> c /= 0) indexed)

        showTerm 0 _ = ""
        showTerm c 0 = show c
        showTerm 1 1 = "x"
        showTerm (-1) 1 = "-x"
        showTerm c 1 = show c ++ "x"
        showTerm 1 p = "x^" ++ show p
        showTerm (-1) p = "-x^" ++ show p
        showTerm c p = show c ++ "x^" ++ show p
    

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P []) (P []) = P []
plus (P coeffs1)  (P coeffs2) = P [plus' coeffs1 coeffs2]
    where
        plus' (x:xs) (y:ys) = (x + y) : plus' xs ys
        plus' [] xs = xs
        plus' ys [] = ys

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate P nums = P (map negate nums)
    fromInteger i = P [fromInteger i]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P (x:xs)) y = x + y + (if null xs then 0 else  applyP (xs) y)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined


