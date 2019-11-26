import Data.List

-- 1. feladat
repeat' :: a -> [a]
repeat' x = xs where xs = x:xs

-- 2. feladat
compress :: Eq a => [a] ->  [(a, Int)]
compress = map (\x -> (head x, length x)) . group 

decompress :: [(a,Int)] -> [a]
decompress = concat . map ((\f (a,b) -> f b a) replicate)