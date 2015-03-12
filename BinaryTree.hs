--Questions 54A - 69, Binary Tree Problems

--Tree Representation
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty --shorthand function

--Examples
tree1 :: Tree Char
tree2 :: Tree Char
tree3 :: Tree Char
tree4 :: Tree Char
tree5 :: Tree Int

tree1 = Empty

tree2 = Branch 'a' Empty Empty

tree3 = leaf 'b'

tree4 = Branch 'a' (Branch 'b' (leaf 'd')
                               (leaf 'e'))
                   (Branch 'c' Empty
                               (Branch 'f' (leaf 'g')
                                           Empty))

tree5 = Branch 1 (Branch 2 Empty
                           (leaf 4))
                 (leaf 2)
--Check the height of a tree
height :: Tree a -> Int

height Empty  = 0
height (Branch _ Empty Empty) = 1
height (Branch _ l r) = 1 + max (height l) (height r)

--Check if a tree is balanced
isBalanced :: Tree a -> Bool

isBalanced Empty = True
isBalanced (Branch _ Empty Empty) = True
isBalanced (Branch _ l r) = abs (height l - height r) <= 1 && isBalanced l
                             && isBalanced r

--Problem 55
cbalTree :: Int -> [Tree Char]

cbalTree 0 = []
cbalTree 1 = [leaf 'x']

--Problem 56

--Problem 57

--Problem 58

--Problem 59

--Problem 60
