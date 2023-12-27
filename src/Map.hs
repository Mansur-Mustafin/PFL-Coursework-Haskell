module Map (Map, empty, insert, find, map2List, isEmpty) where 

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
insert k v Empty = Node (k,v) Empty Empty 0
insert k v (Node (k1,v1) l r bf)
 | k > k1 = let newRight = insert k v r
                newBf = (height l) - (height newRight)
            in balance $ Node (k1,v1) l newRight newBf
 | k < k1 = let newLeft = insert k v l
                newBf = (height newLeft) - (height r)
            in balance $ Node (k1,v1) newLeft r newBf
 | otherwise = Node (k,v) l r bf


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
map2List (Node (k,v) esq dir bf) = map2List esq ++ [(k,v)] ++ map2List dir


-- Calculate the height of a tree
height :: Map k v -> Int 
height Empty = 0
height (Node _ esq dir bf) = 1 + max (height esq) (height dir)


-- Check if a tree is balanced
isBalanced :: Map k v -> Bool
isBalanced Empty = True 
isBalanced (Node (k,v) esq dir bf) 
 | not (isBalanced esq || isBalanced dir) = False
 | abs ((height esq) - (height dir)) > 1 = False
 | otherwise = True 


-- Get left subtree 
leftSubTree :: Map k v -> Map k v
leftSubTree (Node (k,v) esq dir bf) = esq


-- Get right subtree 
rightSubTree :: Map k v -> Map k v
rightSubTree (Node (k,v) esq dir bf) = dir 


-- Get Balance Factor
getBf :: Map k v -> Int 
getBf (Node _ _ _ bf) = bf


-- Left Left Case 
llRotation :: Map k v -> Map k v
llRotation (Node (zk, zv) (Node (yk, yv) x t3 ybf) t4 zbf) =
    Node (yk, yv) x (Node (zk, zv) t3 t4 (zbf - 2)) (ybf - 1)
llRotation _ = error "LL rotation cannot be applied."


-- Right Right Case 
rrRotation :: Map k v -> Map k v
rrRotation (Node (zk, zv) t1 (Node (yk, yv) t2 x ybf) zbf) =
    Node (yk, yv) (Node (zk, zv) t1 t2 (zbf + 2)) x (ybf + 1)
rrRotation _ = error "RR rotation cannot be applied."


-- Balance a tree
balance :: Map k v -> Map k v 
balance (Node (k, v) esq dir bf)
 | bf == 2  && (getBf esq) == 1  = llRotation (Node (k, v) esq dir bf)
 | bf == -2 && (getBf dir) == -1 = rrRotation (Node (k, v) esq dir bf)
 | bf == 2  && (getBf esq) == -1 = llRotation $ Node (k, v) (rrRotation esq) dir bf
 | bf == -2 && (getBf dir) == 1  = rrRotation $ Node (k, v) esq (llRotation dir) bf
 | otherwise = (Node (k, v) esq dir bf)


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
    (250, "Y"), (260, "Z"), (270, "AA"), (280, "BB"),
    (50, "E"), (310, "EE"), (70, "G"), (80, "H"),
    (210, "U"), (220, "V"), (230, "W"), (240, "X"),
    (90, "I"), (100, "J"), (110, "K"), (120, "L"),
    (130, "M"), (140, "N"), (150, "O"), (160, "P"),
    (170, "Q"), (180, "R"), (190, "S"), (200, "T"),
    (290, "CC"), (300, "DD"), (60, "F"), (320, "FF")
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


{-
muchLargerTestTree :: Map Int String
muchLargerTestTree = createTree [
    (10, "A"), (20, "B"), (30, "C"), (40, "D"),
    (250, "Y"), (260, "Z"), (270, "AA"), (280, "BB"),
    (50, "E"), (310, "EE"), (70, "G"), (80, "H"),
    (210, "U"), (220, "V"), (230, "W"), (240, "X"),
    (90, "I"), (100, "J"), (110, "K"), (120, "L"),
    (130, "M"), (140, "N"), (150, "O"), (160, "P"),
    (170, "Q"), (180, "R"), (190, "S"), (200, "T"),
    (290, "CC"), (300, "DD"), (60, "F"), (320, "FF")
    ]
-}