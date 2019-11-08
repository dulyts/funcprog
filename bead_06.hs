import Data.Char (toUpper)

-- 1. feladat
onAxis :: (Num a, Eq a) => (a,a) -> Bool
onAxis (0,_) = True
onAxis (_, 0) = True
onAxis _ = False

-- 2. feladat
punctuation :: Char -> Bool
punctuation '.' = True
punctuation '?' = True
punctuation '!' = True
punctuation _ = False

-- 3. feladat
toUpperThird :: String -> String
toUpperThird (a:b:c:xs) = a:b:(toUpper c):xs
toUpperThird xs = xs
