-- Elso feladat
reszesedes :: Int -> Int
reszesedes x = 2 * x


bevetel :: Int -> Int -> Int
bevetel ar nezo = ar * nezo
-- Masodik feladat
profit :: Bool
profit = fromIntegral(bevetel 6 2645439) * 0.5 > 10000000

-- Harmadik feladat
nemVesztesegesAr :: Double
nemVesztesegesAr = 10000000/0.5/2645439 
