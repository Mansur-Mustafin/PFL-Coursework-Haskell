{-|
Module      : AVLMap
Description : AVL Tree Implementation for Map in Haskell
Copyright   : Daniel Ferreira and Mansur Mustafin, 2023
Maintainer  : unidsferreira2003@gmail.com and mustafin.mansur02@gmail.com

This module 'AVLMap' provides an AVL tree-based map (dictionary) data structure. 
It maintains balance to ensure that operations like insertion, deletion, and lookup 
are efficient. The height of each node is tracked to optimize rebalancing.
-}

module AVLMap (
    -- * Type
    Map,
    -- * Construction
    empty, 
    -- * Operations
    insert, map2List,
    -- * Query
    find, isEmpty
) where


import Data.List (permutations) -- TODO delete this

{-|
    An AVL tree where 'k' is the key, 'v' is the value, and 'Int' is the height of the node.
-}
data Map k v = Empty | Node (k,v) (Map k v) (Map k v) Int
               deriving Show


{-|
    Creates an empty map.
-}
empty :: Map k v
empty = Empty


{-|
    Checks if the map is empty.
-}
isEmpty :: Map k v -> Bool
isEmpty Empty = True
isEmpty _ = False


{-|
   Inserts a key and a value pair into the map. If the key already exists, it updates the value.
   Ensures that the tree remains balanced after insertion.
   
   Complexity: O(log n) on average.
-}
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


{-|
   Finds a value by key in the AVL tree. Returns 'Nothing' if the key is not present, otherwise 'Just' value.

   Complexity: O(log n) on average.
-}
find :: (Ord k) => k -> Map k v -> Maybe v
find k Empty = Nothing
find k (Node (k1,v1) l r _)
 | k < k1 = find k l
 | k > k1 = find k r
 | otherwise = Just v1


{-|
   Converts the map to a list of key-value pairs using Inorder Traversal
   
   Complexity: O(n)
-}
map2List :: Map k v -> [(k,v)]
map2List Empty = []
map2List (Node (k,v) esq dir _) = map2List esq ++ [(k,v)] ++ map2List dir


{-|
    Retrieves the height of a given node in the AVL tree.
-}
height :: Map k v -> Int
height Empty = 0
height (Node _ Empty Empty _) = 1
height (Node _ _ _ h) = h


{-|
    Updates the height of a given node based on its children's heights.
-}
updateHeight :: Map k v -> Int
updateHeight Empty = 0
updateHeight (Node _ Empty Empty _) = 1
updateHeight (Node _ l r _) = 1 + max (height l) (height r)


{-|
    Calculates the balance factor of a node in the AVL tree.
-}
balanceFactor :: Map k v -> Int
balanceFactor Empty = 0
balanceFactor (Node _ l r _) = height l - height r


{-|
    Performs a left-left rotation to balance the AVL tree.
-}
llRotation :: Map k v -> Map k v
llRotation (Node (zk, zv) (Node (yk, yv) x t3 yh) t4 zh) =
    let newZh = 1 + max (height t3) (height t4)
        newYh = 1 + max (height x) newZh
    in Node (yk, yv) x (Node (zk, zv) t3 t4 newZh) newYh
llRotation _ = error "LL rotation cannot be applied."


{-|
    Performs a right-right rotation to balance the AVL tree.
-}
rrRotation :: Map k v -> Map k v
rrRotation (Node (zk, zv) t1 (Node (yk, yv) t2 x yh) zh) =
    let newZh = 1 + max (height t1) (height t2)
        newYh = 1 + max (height x) newZh
    in Node (yk, yv) (Node (zk, zv) t1 t2 newZh) x newYh
rrRotation _ = error "RR rotation cannot be applied."


{-|
   Balances an AVL tree to maintain its properties. 
   It applies rotations based on the balance factor of the node.
-}
balance :: Map k v -> Map k v
balance root = case balanceFactor root of
  2  -> rotateLeftCase root 
  -2 -> rotateRightCase root 
  _  -> root  


{-|
   Handles the case where a left rotation is required. 
    
   - case (1) a simple left-left rotation is performed.
   - case (-1) a double rotation left-right is performed.
-}
rotateLeftCase :: Map k v -> Map k v
rotateLeftCase node@(Node (k, v) esq dir h) = case balanceFactor esq of
  1  -> llRotation node
  -1 -> let rrEsq = rrRotation esq
            newHight = updateHeight (Node (k, v) rrEsq dir 0)
        in llRotation $ Node (k, v) rrEsq dir newHight


{-|
   Handles the case where a right rotation is required. 

   - case (-1) a simple right-right rotation is performed.
   - case (1) a double rotation (right-left) is performed.

   Complexity: O(1), as it performs a constant number of operations.
-}
rotateRightCase :: Map k v -> Map k v
rotateRightCase node@(Node (k, v) esq dir h) = case balanceFactor dir of
  -1 -> rrRotation node
  1  -> let rrDir = llRotation dir
            newHight = updateHeight (Node (k, v) esq rrDir 0)
        in rrRotation $ Node (k, v) esq rrDir newHight



-- ####################################################################################################################
-- #                                                                                                                  #
-- #                                                      Tests                                                       #
-- #                                                                                                                  #
-- ####################################################################################################################


height' :: Map k v -> Int
height' Empty = 0
height' (Node _ Empty Empty _) = 1
height' (Node _ l r _) = 1 + max (height' l) (height' r)

-- Check if a tree is balanced
isBalanced :: Map k v -> Bool
isBalanced Empty = True
isBalanced (Node (k,v) esq dir h)
 | not (isBalanced esq && isBalanced dir) = False
 | abs (height' esq - height' dir) > 1 = False
 | abs (height esq - height dir) > 1 = False
 | otherwise = True

insertIntoTree :: [Int] -> Map Int Int
insertIntoTree = foldl (\t x -> insert x x t) empty

checkBalance :: [Int] -> IO ()
checkBalance permutation =
    if isBalanced (insertIntoTree permutation)
    then return ()
    else putStrLn ("test failed: " ++ show permutation)

main :: IO ()
main = do
    let perms = permutations [1..10]
    mapM_ checkBalance perms
