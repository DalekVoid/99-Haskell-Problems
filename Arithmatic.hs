import Test.QuickCheck
import Data.List
--Problem 31
isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime i = isPrime' i 2

isPrime' :: Int -> Int -> Bool
isPrime' i j
  | i <= j          = True
  | i `mod` j == 0  = False
  | otherwise       = isPrime' i (j+1)

--Problem 32
myGCD :: Integer -> Integer -> Integer

myGCD 0 0 = 0
myGCD _ 0 = 1 
myGCD i j
  | i < j      = myGCD j i
  | i == j     = i
  | otherwise  = myGCD (i-j) j

prob_myGCD :: Integer -> Integer -> Bool
prob_myGCD i j = myGCD i j == gcd i j

--Problem 33
myCoprime :: Integer -> Integer -> Bool
myCoprime i j = gcd i j == 1
 
--Problem 34
totientPhi :: Integer -> Int 

totientPhi 1 = 1
totientPhi i = length $ filter (myCoprime i) [1..i-1]

--Problem 35
primeFactors :: Int -> [Int]
primeFactors i = primeFactors' i 2

primeFactors' :: Int -> Int -> [Int]

primeFactors' 1 _ = []
primeFactors' i x
  | i `mod` x == 0 = x : primeFactors' (i`div`x) x 
  | otherwise      = primeFactors' i (nextPrime x)

nextPrime :: Int -> Int
nextPrime 0 = 2
nextPrime 1 = 2
nextPrime i
  | isPrime (succ i) = succ i 
  | otherwise   = nextPrime (succ i)
 
--Problem 36
--TODO Handle zero case
--TODO Handle very large Integer, possible causeing overflow
countList :: [a] -> (a, Int)
countList [] = error "empty list"
countList (x:xs) = (x, length(x:xs)) 

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult x = map countList ((group.primeFactors) x)

--Problem 37
improvedTotientPhi :: Int -> Int
improvedTotientPhi x = foldr1 (*) [(p-1)*p^(m-1)|(p, m) <- primeFactorsMult x]

--Problem 39
primeList :: Int -> Int -> [Int]
primeList first final = [x | x <- [first, first+1..final], isPrime x]

--Problem 40
goldbach :: Int -> (Int, Int)
goldbach n
  | n <= 2         = error "Cannot be smaller than 3"
  | n `mod` 2 == 1 = error "Cannot be odd number" 
  | otherwise      = head [(x, y) | x <- primeList 2 (n-2), let y = n-x, isPrime y]

--Problem 41
goldbachList :: Int -> Int -> [(Int , Int)]
goldbachList first final = [goldbach x| x <- [first, first+1..final], x`mod`2==0]
