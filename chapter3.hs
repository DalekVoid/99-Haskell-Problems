import System.Random
import Control.Monad (replicateM)

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
  | otherwise = replicateM i selected
     where
       selected = do r <- randomRIO(0, len)
                     return (list!!r) 
       len = length list - 1

randomSelectFrom :: Int -> Int -> IO [Int]
randomSelectFrom count limit = randomSelect (range 1 limit) count

permutations :: [a] -> [[a]]
permutations []          = [[]]
permutations [x]         = [[x]]
