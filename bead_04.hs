
-- 1. feladat:
sumSquaresTo :: Integer -> Integer
sumSquaresTo n = sum [x ^ 2 | x <- [1..n]]

-- 2. feladat:
startingLetters :: String -> [Char]
startingLetters t = [head x | x<- (words t)]

-- 3. feladat:
underAttack x y
    | fst x == fst y = True
    | snd x == snd y = True
    | abs (fst x - fst y) == abs (snd x - snd y) = True
    | otherwise = False
countAttackingQueens :: Integral a => (a,a) -> [(a,a)] -> a
countAttackingQueens selected queens = sum [ 1 | x <- queens, (underAttack selected x)]
