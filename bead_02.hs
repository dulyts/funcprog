-- 1)
add :: (Num a, Num b) => (a,b) -> (a,b) -> (a,b)
add (a,b) (c,d) = (a+c, b+d)

sub :: (Num a, Num b) => (a,b) -> (a,b) -> (a,b)
sub (a,b) (c,d) = (a-c, b-d)

-- 2)
mul :: (Num a) => a -> (a,a) -> (a,a)
mul a (b,c) = (a*b, a*c)

-- 3)
scalar :: (Num a) => (a,a) -> (a,a) -> a
scalar (a1,b1) (a2,b2) = a1*a2+b1*b2

-- 4)
vecLength :: (Floating a) => (a,a) -> a
vecLength (a,b) = sqrt(a*a + b*b)

-- 5)
pairToList :: (a,a) -> [a]
pairToList (a,b) = [a,b]
