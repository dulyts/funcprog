-- 0. feladat
module Bead where

    -- 1. feladat
    data Time = T Int Int deriving (Eq)

    -- 2. feladat
    t :: Int -> Int -> Time
    t x y | x < 0 || x > 23 || y < 0 || y > 59 = error "Invalid Time params"
        | otherwise                          = T x y

    -- 3. feladat
    instance Show Time where
        -- show :: Time -> String; ugye ez lesz a típusa, de ezt a sort nem szabad odaírni, ez magában a Show osztályban már ott van.
        show (T x y) = (show x) ++ ":" ++ (show y)

    instance Ord Time where
        (<) (T o1 p1) (T o2 p2) = o1 < o2 || ( o1 == o2 && p1 < p2)
        (<=) (T o1 p1) (T o2 p2) = o1 < o2 || ( o1 == o2 && p1 <= p2)

    -- 4. feladat
    isEarlier :: Time -> Time -> Bool
    -- isEarlier (T o1 p1) (T o2 p2) = o1 < o2 || ( o1 == o2 && p1 < p2)
    isEarlier t1 t2 = t1 < t2

    -- 5. feladat
    isBetween :: Time -> Time -> Time -> Bool
    isBetween t1 t2 t3 = (t1 <= t2 && t2 <= t3) || (t3 <= t2 && t2 <= t1)

    -- 6. feladat
    data Meridiam = Am | Pm deriving (Eq, Enum)
    instance Show Meridiam where
        show Am = "AM"
        show Pm = "PM"

    data USTime = UT Meridiam Int Int deriving (Eq)

    -- 7. feladat
    ustime :: Meridiam -> Int -> Int -> USTime
    ustime m x y | x < 0 || x > 23 || y < 0 || y > 59 = error "Invalid Time params"
                | otherwise                          = UT m x y

    -- 8. feladat
    instance Show USTime where
        show (UT m x y) = (show m) ++ " " ++ (show x) ++ ":" ++ (show y) 

    -- 9a. feladat
    ustimeToTime :: USTime -> Time
    ustimeToTime (UT m x y) = T (fromIntegral (12 *(fromEnum m) + (x `mod` 12))) y

    -- 9b. feladat
    timeToUSTime :: Time -> USTime
    timeToUSTime (T x y) = UT (toEnum (x `div` 12)) (12 - ((- x) `mod` 12)) y
