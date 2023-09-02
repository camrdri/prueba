-- ejer 1a
f :: Integer -> Integer
f x | x==1 = 8
    | x==4 = 131
    | x==16 = 16
-- ejer 1b
g :: Integer -> Integer
g x | x==8 = 16
    | x==16 = 4
    | x==131 = 1
-- ejer 1c
h :: Integer -> Integer
h x = f (g x)
k :: Integer -> Integer
k x = g (f x)

-- ejer 2a
absoluto :: Int -> Int
absoluto x | x>=0 = x
           | otherwise = -x
-- ejer 2b
maximoabsoluto :: Int -> Int -> Int
maximoabsoluto x y | absoluto x < absoluto y = absoluto y
                   | otherwise = absoluto x
--ejer 2c
maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | x>y && x>z = x
              | y>x && y>z = y
              | x==y && x>z = x
              | otherwise = z
-- ejer 2d
algunoes0 :: Float -> Float -> Bool
algunoes0 x y | x==0 = True
              | y==0 = True| otherwise = False
--ejer 2e
ambosson0 :: Float -> Float -> Bool
ambosson0 x y | x==0 && y==0 = True
              | otherwise = False
--ejer 2f 
mismointervalo :: Float -> Float -> Bool
mismointervalo x y | x<=3 && y<=3 = True
                   | x>7 && y>7 = True
                   | 3<x && x<=7 && 3<y && y<=7 = True
                   | otherwise = False
-- ejer 2g
sumadistintos :: Int -> Int -> Int -> Int
sumadistintos x y z | x==y && y==z = x
                    | x/=y && z/=x && y/=z = x+z+y
                    | x==y = z+x
                    | x==z = x+y
                    | otherwise = x+y
-- ejer 2h
esmultiplode :: Int -> Int -> Bool
esmultiplode x y = (mod x y == 0)
-- ejer 2i
digitounidades :: Int -> Int
digitounidades x = mod (absoluto x) 10
-- ejer 2j
digitodecenas :: Int -> Int
digitodecenas x = div (mod (absoluto x) 100 - mod (absoluto x) 10) 10

-- ejer 3
estanrelacionados :: Integer -> Integer -> Bool
estanrelacionados x y = (mod (x*x) (x*y) == 0)

-- ejer 4a
prodint :: (Num t) => (t,t) -> (t,t) -> t
prodint (x1, y1) (x2, y2) = (x1*x2 + y1*y2)
-- ejer 4b
todomenor :: (Ord t) => (t,t) -> (t,t) -> Bool
todomenor (x1, y1) (x2, y2) = (x1<x2 && y1<y2)
-- ejer 4c
distanciapuntos :: (Floating t) => (t,t) -> (t,t) -> t
distanciapuntos (x1, y1) (x2, y2) = sqrt ((x2-x1)**2 + (y2-y1)**2)
-- ejer 4d
sumaterna :: (Int, Int, Int) -> Int
sumaterna (x, y, z) = x+z+y
-- ejer 4e
sumasolomultiplos :: (Int, Int, Int) -> Int -> Int
sumasolomultiplos (x, y, z) w | (mod x w == 0 && mod y w == 0 && mod z w == 0) = x + y + z
                              | (mod x w /= 0 && mod y w /= 0 && mod z w /= 0) = 0
                              | (mod x w == 0 && mod y w == 0) = x+y
                              | (mod z w == 0 && mod x w == 0) = x+z
                              | (mod x w == 0) = x
                              | (mod z w == 0 && mod y w == 0) = y+z
                              | (mod z w == 0) = z
                              | otherwise = y
-- ejer 4f
posprimerpar :: (Int, Int, Int) -> Int
posprimerpar (x, y, z) | mod x 2 == 0 = 1
                       | mod y 2 == 0 = 2
                       | mod z 2 == 0 = 3
                       | otherwise = 4
-- ejer 4g
crearpar :: a -> b -> (a,b)
crearpar x y = (x,y)
-- ejer 4h
invertir :: (a, b) -> (b, a)
invertir (x, y) = (y, x)

-- ejer 5
a :: Integer -> Integer
a x | x<=7 = x*x
    | otherwise = 2*x-1
b :: Integer -> Integer
b x | mod x 2 == 0 = div x 2
    | otherwise = 3*x+1
todosmenores :: (Integer, Integer, Integer) -> Bool
todosmenores (x, y, z) = ((a x) > (b x) && (a y) > (b y) && (a z) > (b z))

-- ejer 6
bisiesto :: Integer -> Bool
bisiesto x | mod x 4 /= 0 = False
           | (mod x 100 == 0 && mod x 400 /= 0) = False
           | otherwise = True

-- ejer 7
distanciamanhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciamanhattan (x1, y1, z1) (x2, y2, z2) | ((x1-x2)+(y1-y2)+(z1-z2))>=0 = (x1-x2)+(y1-y2)+(z1-z2)
                                             | otherwise = -1*((x1-x2)+(y1-y2)+(z1-z2))

-- ejer 8
sumaultimosdosdigitos :: Integer -> Integer
sumaultimosdosdigitos x = (mod x 10 + mod (div x 10) 10) 
comparar :: Integer -> Integer -> Integer
comparar x y | sumaultimosdosdigitos x < sumaultimosdosdigitos y = 1
             | sumaultimosdosdigitos x > sumaultimosdosdigitos y = -1
             | otherwise = 0

-- :)