-- 1. feladat
divides :: Integral a => a -> a -> Bool
divides 0 0 = True
divides 0 _ = False
divides a b = b `mod` a == 0

-- 2. feladat
isDoubleton :: [a] -> Bool
isDoubleton [_,_] = True
isDoubleton _ = False

-- 3. feladat
nand :: Bool -> Bool -> Bool
nand True b = not b
nand _ _ = True