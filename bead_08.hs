-- 1a. feladat:
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

-- 1b. feladat:
(!!!) :: Integral i => [a] -> i -> a
(!!!) l i
    -- | i < 0 = negative (reverse' l) (-1 * i-1)
    -- | otherwise = positive l i
    | i < 0     = helper (reverse' l) (-1 * i-1) "Túl kicsi index!"
    | otherwise = helper l i "Túl nagy index!"
    where 
        helper []     _ e = error e
        helper (x:_)  0 e = x
        helper (x:xs) i e = helper xs (i-1) e

-- 2. feladat:
format :: Integral i => i -> [Char] -> [Char]
format n l
    | n < 0     = formatHelper 0 l
    | otherwise = formatHelper n l
    where
        formatHelper 0 [] = []
        formatHelper 0 (x:xs) = x : formatHelper 0 xs
        formatHelper n []     = " " ++ formatHelper (n-1) []
        formatHelper n (x:xs) = x : formatHelper (n-1) xs

-- 3. feladat:
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ []     = []
splitOn s l = helper l
    where
        helper []     = [[]]
        helper (x:xs)
            | s == x            = [] : helper xs
            | otherwise         = (x : head (helper xs)): tail (helper xs)

-- 4. feladat:
emptyLines :: Integral i => [Char] -> [i]
emptyLines "" = [1]
emptyLines s  = [a | (a,b) <- zip [1..] (splitOn '\n' s), b == ""]

-- 5. feladat:
csv :: String -> [[String]]
csv s = ([ splitOn ',' line | line <- splitOn '\n' s])
