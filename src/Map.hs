module Map (Map, empty, insert, find, map2List, isEmpty) where 

-- TODO add garbage collection
data Map k v = Empty | Node (k,v) (Map k v) (Map k v) deriving Show

-- Crate empty tree
empty :: Map k v
empty = Empty


-- Check if tree is empty
isEmpty :: Map k v -> Bool 
isEmpty Empty = True 
isEmpty _ = False


-- Insert or update element in a tree
insert :: (Ord k) => k -> v -> Map k v -> Map k v
insert k v Empty = Node (k,v) Empty Empty
insert k v (Node (k1,v1) l r)
 | k > k1 = balance $ Node (k1,v1) l (insert k v r)
 | k < k1 = balance $ Node (k1,v1) (insert k v l) r
 | otherwise = Node (k,v) l r


-- Find element in a tree 
find :: (Ord k) => k -> Map k v -> Maybe v
find k Empty = Nothing
find k (Node (k1,v1) l r)
 | k < k1 = find k l
 | k > k1 = find k r 
 | otherwise = Just v1 


-- Inorder Traversal
map2List :: Map k v -> [(k,v)]
map2List Empty = []
map2List (Node (k,v) esq dir) = map2List esq ++ [(k,v)] ++ map2List dir


-- Calculate the height of a tree
height :: Map k v -> Int 
height Empty = 0
height (Node _ esq dir) = 1 + max (height esq) (height dir)


-- Check if a tree is balanced
isBalanced :: Map k v -> Bool
isBalanced Empty = True 
isBalanced (Node (k,v) esq dir) 
 | not (isBalanced esq || isBalanced dir) = False
 | abs ((height esq) - (height dir)) > 1 = False
 | otherwise = True 


-- Get left subtree 
leftSubTree :: Map k v -> Map k v
leftSubTree (Node (k,v) esq dir) = esq


-- Get right subtree 
rightSubTree :: Map k v -> Map k v
rightSubTree (Node (k,v) esq dir) = dir 


-- Simple Left Rotation of a tree
leftRotation :: Map k v -> Map k v 
leftRotation (Node (zk,zv) t1 (Node (yk, yv) t2 x)) 
 = Node (yk, yv) (Node (zk, zv) t1 t2) x 
leftRotation _ = error "deu merda"


-- Simple Right Rotation of a tree
rightRotation :: Map k v -> Map k v 
rightRotation (Node (zk,zv) (Node (yk, yv) x t3) t4) 
 = Node (yk, yv) x (Node (zk, zv) t3 t4)
rightRotation _ = error "deu merda"


-- Balance a tree
balance :: Map k v -> Map k v 
balance (Node (k, v) esq dir)
 | balanceIndex == 2  = if height (leftSubTree esq) < height (rightSubTree esq)
                        then rightRotation (Node (k, v) esq dir)
                        else rightRotation $ Node (k, v) (leftRotation esq) dir 
 | balanceIndex == -2 = if height (leftSubTree dir) < height (rightSubTree dir)
                        then leftRotation (Node (k, v) esq dir)
                        else leftRotation $ Node (k, v) esq (rightRotation dir)
 | otherwise = (Node (k, v) esq dir)
 where 
  balanceIndex = (height esq) - (height dir)





-- ####################################################################################################################
-- #                                                                                                                  #
-- #                                                      Tests                                                       #
-- #                                                                                                                  #
-- ####################################################################################################################

createTree :: [(Int, String)] -> Map Int String
createTree = foldl (flip $ uncurry insert) empty

-- A larger test tree
largerTestTree :: Map Int String
largerTestTree = createTree [(10, "j"), (20, "k"), (30, "l"), (40, "m"), (50, "n"), (60, "o"), (70, "p"), (80, "q")]

muchLargerTestTree :: Map Int String
muchLargerTestTree = createTree [
    (10, "A"), (20, "B"), (30, "C"), (40, "D"),
    (50, "E"), (60, "F"), (70, "G"), (80, "H"),
    (90, "I"), (100, "J"), (110, "K"), (120, "L"),
    (130, "M"), (140, "N"), (150, "O"), (160, "P"),
    (170, "Q"), (180, "R"), (190, "S"), (200, "T"),
    (210, "U"), (220, "V"), (230, "W"), (240, "X"),
    (250, "Y"), (260, "Z"), (270, "AA"), (280, "BB"),
    (290, "CC"), (300, "DD"), (310, "EE"), (320, "FF")
    ]

-- Test insertion
testInsertion :: IO ()
testInsertion = do
    let tree = insert 25 "new" largerTestTree
    case find 25 tree of
        Just "new" -> putStrLn "Passed: Insertion."
        _          -> putStrLn "Failed: Insertion."


-- Test find
testFind :: IO ()
testFind = do
    let tree = largerTestTree
    case find 50 tree of
        Just "n" -> putStrLn "Passed: Find."
        _        -> putStrLn "Failed: Find."

-- Test deletion
{-
testDeletion :: IO ()
testDeletion = do
    let tree = delete 10 largerTestTree
    case find 10 tree of
        Nothing -> putStrLn "Passed: Deletion."
        _       -> putStrLn "Failed: Deletion."
-}

runAllTests :: IO ()
runAllTests = do
    testInsertion
    testFind
    -- testDeletion



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