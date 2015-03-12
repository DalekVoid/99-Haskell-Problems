{-# LANGUAGE NoImplicitPrelude #-}

import Prelude
import Control.Applicative
import Data.List hiding (head, tail, init, last, (!!))
import qualified System.Random as R
import qualified Control.Monad as M (replicateM, return)

-- Problem 1
myLast :: [a] -> a
myLast = foldl1' (flip const)

-- Problem 2
myButLast :: [a] -> a

myButLast [x, _] = x
myButLast [x]    = x
myButLast (_:xs) = myButLast xs

-- Problem 3
elementAt :: [a] -> Integer -> a
elementAt (x:xs) count
  | count < 1  = error "Overflow"
  | count == 1 = x
  | count > 1  = elementAt xs (count-1)

-- Problem 4
myLength :: [a] -> Integer
myLength = foldl' (const.succ) 0

--Problem 5
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

--Problem 6 v1
isPalindrome ::(Eq a) => [a] -> Bool
isPalindrome x = x == myReverse x

--Problem 7
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]

myFlatten (Elem a) = [a]
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)
myFlatten (List []) = []

--Problem 8
compress :: Eq a => [a] -> [a]
compress = undefined

-- compress = foldl' (\acc x -> if myLast acc == x then acc ++ [x] else acc) []

{-
compress (x:xs) = compress' [] (x:xs)

compress' :: Eq a => [a] -> [a] -> [a]

compress' [] [] = []
compress' xs [] = xs
compress' [] (y:ys) = compress' [y] ys
compress' xs (y:ys)
  | y /= last xs = compress' (xs++[y]) ys
  | otherwise    = compress' xs ys
  -}

--Probelm 9
pack :: Eq a => [a] -> [[a]]

pack [] = []
pack xx = pack' [] xx

pack' :: Eq a => [[a]] -> [a] -> [[a]]
pack' xx []   = xx
pack' [] (y:ys) = pack' [[y]] ys
pack' xx (y:ys)
  | last xx `myContain` y = pack' (init xx ++ [last xx++[y]]) ys
  | otherwise             = pack' (xx++[[y]]) ys
pack' _ _     = []

myContain :: Eq a => [a] -> a -> Bool

myContain [] y = False
myContain (x:xs) y
  | x == y = True
  | otherwise = myContain xs y

--Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map ((,) <$> length <*> head) . pack
--Problem 14
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x, x])

--Problem 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

--Problem 16
myDrop :: [a] -> Int -> [a]

myDrop [] _ = []
myDrop l@(x:xs) i
  | length l `mod` i == 0 = myDrop xs i
  | otherwise             = x : myDrop xs i

--Problem 17
split :: [a] -> Int -> ([a], [a])
split l = split' ([], l)

split' :: ([a], [a]) -> Int -> ([a], [a])
split' (l1, l2) 0 = (l1, l2)
split' (l1, []) _ = (l1, [])
split' (l1, y:ys) i = split' (l1++[y], ys) (i-1)

--Problem 18
slice :: [a] -> Integer -> Integer -> [a]
slice l i j
  | j >= i = slice' l i (j-i+1)
  | otherwise  = []

slice' :: [a] -> Integer -> Integer -> [a]

slice' [] i j = []
slice' (x:xs) i j
  | i > 1          = slice' xs (i-1) j
  | i == 1 && j == 0= []
  | i == 1 && j > 0 = x : slice' xs i (j-1)
  | otherwise       = []

--Problem 19
rotate :: [a] -> Int -> [a]
rotate list n = second ++ first
  where (first, second) = split list n

{-
rotate l 0 = l
rotate l@(x:xs) i
  | i > 0 = rotate (xs++[x]) (i-1)
  | i < 0 = rotate l (length l + i)
  -}

--Problem 20
removeAt :: [a] -> Integer -> [a]

removeAt [] _ = []
removeAt l@(x:xs) i
  | i <= 0          = l
  | i == 1          = xs
  | otherwise       = x : removeAt xs (i-1)



insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt x list@(y:ys) i
  | i <= 0    = error "position must be greater than 0"
  | i == 1    = x : list
  | otherwise = y : insertAt x ys (i-1)

range :: Int -> Int -> [Int]
range left right
  | left > right = []
  | otherwise    = left : range (left+1) right

randomSelect :: [a] -> Int -> IO [a]
randomSelect _ 0 = return []
randomSelect list i
  | i < 0     = error "count should be greater than zero"
  | otherwise = M.replicateM i selected
     where
       selected = do r <- R.randomRIO(0, len)
                     return (list!!r)
       len = length list - 1

randomSelectFrom :: Int -> Int -> IO [Int]
randomSelectFrom count limit = randomSelect (range 1 limit) count

permutations :: [a] -> [[a]]
permutations []          = [[]]
permutations [x]         = [[x]]
