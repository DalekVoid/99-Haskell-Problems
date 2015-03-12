import Control.Applicative
import Data.List

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
  | (last xx) `myContain` y = pack' (init xx ++ [last xx++[y]]) ys
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
