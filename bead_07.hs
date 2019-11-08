-- 1. feladat:
lucas :: Int -> Int
lucas 0 = 2
lucas 1 = 1
lucas n = lucas (n-2) + lucas (n-1)

-- 2. feladat:
longerThan :: Integral i => [a] -> i -> Bool
longerThan []     n = 0 > n
longerThan (x:xs) n = n < 0 || longerThan xs (n-1)

-- 3. feladat:
format :: Integral i => i -> [Char] -> [Char]
format 0 [] = []
format 0 (x:xs) = x : format 0 xs
format n []     = " " ++ format (n-1) []
format n (x:xs) = x : format (n-1) xs