import Data.List
import Data.Function
import Data.Char


toTuple :: String -> (Char, Int)
toTuple x = ((head x), length x)

-- 1. feladat:
compress :: String -> [(Char, Int)]
compress t = map (toTuple) $ group t


-- 2. feladat:
decompress :: [(Char, Int)] -> String
decompress l = concat [replicate (snd x) (fst x) | x <- l]

-- 3. feladat:
mountain :: Integer -> String
mountain n = unlines [ concat [genericReplicate x '#'] | x <- [1..n]]
