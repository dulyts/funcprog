-- 1. feladat:
mapping :: [(Char,Char)]
mapping =  shift 3 (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'])
    where
        shift n list = zip list (drop n list ++ take n list)

-- 2. feladat:
encodeCaesar :: String -> String
encodeCaesar s = map convert s
    where
        convert x = snd (head (filter (\(a,b) -> a == x) mapping))

-- 3. feladat:
decodeCaesar :: String -> String
decodeCaesar s = map convert s
    where
        convert x = fst (head (filter (\(a,b) -> b == x) mapping))

-- 4. feladat:
lucas :: [Integer]
lucas = 2 : calc
    where calc = 1 : 3 : zipWith (+) calc (tail calc)

-- 5. feladat:
apsOnLists :: [(a->b)] -> [[a]] -> [[b]]
apsOnLists fns lists = map (\(a,b) -> map a b) (zip fns lists)