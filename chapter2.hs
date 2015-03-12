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
split l i = split' ([], l) i

split' :: ([a], [a]) -> Int -> ([a], [a])
split' (l1, l2) 0 = (l1, l2)
split' (l1, []) _ = (l1, [])
split' (l1, (y:ys)) i = split' (l1++[y], ys) (i-1)

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
