-- exercise 2
-- my implementation of BST tree with some useful functions


data Tree a = Empty | Node (Tree a) a (Tree a)
              deriving (Show, Eq)

insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty val = Node Empty val Empty
insert (Node left x right) val
    | x == val = Node left x right
    | val > x = Node left x (insert right val)
    | otherwise = Node (insert left val) x right


empty :: (Eq a) => Tree a -> Bool
empty tree = if tree == Empty then True else False 


maxTree :: (Ord a) => Tree a -> a
maxTree Empty = error "Empty tree"
maxTree (Node left x right) = max x (max y z)
    where 
        y = if(left == Empty) then x else maxTree left
        z = if(right ==Empty) then x else maxTree right
  

minTree :: (Ord a) => Tree a -> a
minTree Empty = error "Empty tree"
minTree (Node left x right) = min x (min y z)
    where 
        y = if(left == Empty) then x else minTree left
        z = if(right ==Empty) then x else minTree right

        
isBinary :: (Ord a) => Tree a -> Bool
isBinary Empty = True
isBinary (Node left x right) = 
    isBinary left &&
    isBinary right &&
    (left == Empty || maxTree left < x) &&
    (right == Empty || minTree right > x)


search :: (Ord a) => Tree a -> a -> Bool
search (Node left x right) val  
    | x == val = True
    | val > x && right /= Empty = search right val
    | val < x && left /= Empty = search left val
    | otherwise = False 

height :: Tree a -> Int
height Empty = 0
height (Node left x right) = 1 + max (height left) (height right)

isBalanced :: (Ord a) => Tree a -> Bool
isBalanced Empty = error "Empty tree"
isBalanced (Node left x right) = if (height left) - (height right) <= 1 then True else False


traverseVLR :: Tree a -> [a]
traverseVLR Empty = []
traverseVLR (Node left x right) = [x] ++ traverseVLR left ++ traverseVLR right

traverseLVR :: Tree a -> [a]
traverseLVR Empty = []
traverseLVR (Node left x right) = traverseLVR left ++ [x] ++ traverseLVR right

traverseLRV :: Tree a -> [a]
traverseLRV Empty = []
traverseLRV (Node left x right) = traverseLRV left ++ traverseLRV right ++ [x]

traverseVRL :: Tree a -> [a]
traverseVRL Empty = []
traverseVRL (Node left x right) = [x] ++ traverseVRL right ++ traverseVRL left

traverseRVL :: Tree a -> [a]
traverseRVL Empty = []
traverseRVL (Node left x right) = traverseRVL right ++ [x] ++ traverseRVL left

traverseRLV :: Tree a -> [a]
traverseRLV Empty = []
traverseRLV (Node left x right) = traverseRLV right ++ traverseRLV left ++ [x]


toString :: (Show a,Eq a) => Tree a -> String 
toString Empty = ""
toString (Node left x right) = 
    if left == Empty && right == Empty then show x 
    else show x ++ "(" ++ (toString left) ++ "," ++ (toString right) ++ ")"

leaves :: (Eq a) => Tree a -> [a]
leaves Empty = []
leaves (Node left x right) 
    | left == Empty && right == Empty = [x]
    | otherwise = [] ++ leaves left ++ leaves right

nnodes :: Tree a -> Int
nnodes Empty = 0
nnodes (Node left x right) = nnodes left + nnodes right + 1

nsum :: (Eq a, Num a) => Tree a -> a
nsum (Node left x right)
    | left == Empty && right == Empty = x
    | left == Empty = x + nsum right
    | right == Empty = x + nsum left
    | otherwise = x + nsum left + nsum right

tmap :: (a -> b) -> Tree a -> Tree b
tmap foo Empty = Empty
tmap foo (Node left x right) = Node (tmap foo left) (foo x) (tmap foo right)


removeMax :: (Ord a) => Tree a -> (a, Tree a)
removeMax (Node left x Empty) = (x, left)
removeMax (Node left x right) = (y, Node left x ty)
    where (y, ty) = removeMax right

remove :: (Ord a) => Tree a -> a -> Tree a
remove Empty val = Empty
remove (Node left x right) val
    | val < x = Node (remove left val) x right
    | val > x = Node left x (remove right val)
    | otherwise = if ( left == Empty) then right
                  else (Node ty y right)
      where (y, ty) = removeMax left

merge :: (Num a) => Tree a -> Tree a -> Tree a
merge Empty tree = tree
merge tree Empty = tree
merge (Node left x right) (Node left1 x1 right1) =
    Node (merge left left1) (x+x1) (merge right right1)


tree1 = Node (Node (Node (Node Empty 1 Empty) 2 Empty) 3 (Node Empty 6 Empty )) 8 (Node Empty 10 Empty)
tree2 = Node (Node (Node (Node Empty 1 Empty) 2 Empty) 3 (Node Empty 6 Empty )) 8 (Node (Node Empty 1 Empty) 10 Empty)

--             tree 1             tree 2
--
--              (8)                (8)
--            /     \            /     \
--          (3)     (10)       (3)     (10)
--          / \                / \     /
--        (2) (6)            (2) (6) (1)
--        /                  /  
--      (1)                (1)




