module Map (Map, empty, insert, find, map2List, isEmpty) where 

import Data.List (permutations)

-- TODO add garbage collection
data Map k v = Empty | Node (k,v) (Map k v) (Map k v) Int deriving Show

-- Crate empty tree
empty :: Map k v
empty = Empty


-- Check if tree is empty
isEmpty :: Map k v -> Bool 
isEmpty Empty = True 
isEmpty _ = False


-- Insert or update element in a tree
insert :: (Ord k) => k -> v -> Map k v -> Map k v
insert k v Empty = Node (k,v) Empty Empty 1
insert k v (Node (k1,v1) l r h)
 | k > k1 = let newRight = insert k v r
                newHeightR = updateHeight (Node (k1,v1) l newRight 0)
            in balance $ Node (k1,v1) l newRight newHeightR
 | k < k1 = let newLeft = insert k v l
                newHeight = updateHeight (Node (k1,v1) newLeft r 0)
            in balance $ Node (k1,v1) newLeft r newHeight
 | otherwise = Node (k,v) l r h


-- Find element in a tree 
find :: (Ord k) => k -> Map k v -> Maybe v
find k Empty = Nothing
find k (Node (k1,v1) l r _)
 | k < k1 = find k l
 | k > k1 = find k r 
 | otherwise = Just v1 


-- Inorder Traversal
map2List :: Map k v -> [(k,v)]
map2List Empty = []
map2List (Node (k,v) esq dir _) = map2List esq ++ [(k,v)] ++ map2List dir


-- Calculate the height of a tree
height :: Map k v -> Int 
height Empty = 0
height (Node _ Empty Empty _) = 1
height (Node _ _ _ h) = h

height' :: Map k v -> Int 
height' Empty = 0
height' (Node _ Empty Empty _) = 1
height' (Node _ l r _) = 1 + max (height' l) (height' r)

updateHeight :: Map k v -> Int 
updateHeight Empty = 0
updateHeight (Node _ Empty Empty _) = 1
updateHeight (Node _ l r _) = 1 + max (height l) (height r)


-- Check if a tree is balanced
isBalanced :: Map k v -> Bool
isBalanced Empty = True 
isBalanced (Node (k,v) esq dir h) 
 | not (isBalanced esq && isBalanced dir) = False
 | abs ((height' esq) - (height' dir)) > 1 = False
 | abs ((height esq) - (height dir)) > 1 = False
 | otherwise = True 


-- Get left subtree 
leftSubTree :: Map k v -> Map k v
leftSubTree (Node (k,v) esq _ _) = esq


-- Get right subtree 
rightSubTree :: Map k v -> Map k v
rightSubTree (Node (k,v) _ dir _) = dir 


balanceFactor :: Map k v -> Int
balanceFactor Empty = 0
balanceFactor (Node _ l r _) = (height l) - (height r)


-- Left Left Case 
llRotation :: Map k v -> Map k v
llRotation (Node (zk, zv) (Node (yk, yv) x t3 yh) t4 zh) =
    let newZh = 1 + max (height t3) (height t4)
        newYh = 1 + max (height x) newZh
    in Node (yk, yv) x (Node (zk, zv) t3 t4 newZh) newYh
llRotation _ = error "LL rotation cannot be applied."


-- Right Right Case 
rrRotation :: Map k v -> Map k v
rrRotation (Node (zk, zv) t1 (Node (yk, yv) t2 x yh) zh) =
    let newZh = 1 + max (height t1) (height t2)
        newYh = 1 + max (height x) newZh
    in Node (yk, yv) (Node (zk, zv) t1 t2 newZh) x newYh
rrRotation _ = error "RR rotation cannot be applied."


-- Balance a tree
balance :: Map k v -> Map k v 
balance (Node (k, v) esq dir h)
 | bf == 2  && (balanceFactor esq) == 1  = llRotation (Node (k, v) esq dir h)
 | bf == -2 && (balanceFactor dir) == -1 = rrRotation (Node (k, v) esq dir h)
 | bf == 2  && (balanceFactor esq) == -1 = let 
                                            rrEsq = (rrRotation esq)
                                            newHight = updateHeight (Node (k, v) rrEsq dir 0)
                                           in llRotation $ Node (k, v) rrEsq dir newHight
 | bf == -2 && (balanceFactor dir) == 1  = let 
                                            rrDir = llRotation dir
                                            newHight = updateHeight (Node (k, v) esq rrDir 0)
                                           in rrRotation $ Node (k, v) esq rrDir newHight
 | otherwise = (Node (k, v) esq dir h)
 where bf = balanceFactor (Node (k, v) esq dir h)

-- ####################################################################################################################
-- #                                                                                                                  #
-- #                                                      Tests                                                       #
-- #                                                                                                                  #
-- ####################################################################################################################


insertIntoTree :: [Int] -> Map Int Int
insertIntoTree = foldl (\t x -> insert x x t) empty

checkBalance :: [Int] -> IO ()
checkBalance permutation = 
    if isBalanced (insertIntoTree permutation) 
    then return ()
    else putStr ("test failed: " ++ (show permutation) ++ "\n") 

main :: IO ()
main = do
    let perms = permutations [1..11]
    mapM_ checkBalance perms 


{-

    T1, T2, T3 and T4 are subtrees.
             z                                      y 
            / \                                   /   \
           y   T4      Right Rotate (z)          x      z
          / \          - - - - - - - - ->      /  \    /  \ 
         x   T3                               T1  T2  T3  T4
        / \
      T1   T2


      z                                y
     /  \                            /   \ 
    T1   y     Left Rotate(z)       z      x
        /  \   - - - - - - - ->    / \    / \
       T2   x                     T1  T2 T3  T4
           / \
         T3  T4

-}

