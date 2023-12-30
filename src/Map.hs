{-|
Module      : Map
Description : Binary Search Tree Implementation for Map in Haskell
Copyright   : who
Maintainer  : who@gmail.com

This module 'Map' provides a simple binary search tree based map (dictionary) data structure.
It allows to perform operations like insertion, and lookup in logarithmic time.
-}

module Map (
    -- * Type
    Map,
    -- * Construction
    empty,
    -- * Operations
    insert, map2List,
    -- * Query
    find, isEmpty,
) where

{-|
    A binary search tree based map, where 'k' is the key and 'v' is the value.
-}
data Map k v = Empty | Node (k,v) (Map k v) (Map k v) 
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
   
   Complexity: O(log n) on average.
-}
insert :: (Ord k) => k -> v -> Map k v -> Map k v
insert k v Empty = Node (k,v) Empty Empty
insert k v (Node (k1,v1) l r)
 | k > k1 = Node (k1,v1) l (insert k v r)
 | k < k1 = Node (k1,v1) (insert k v l) r
 | otherwise = Node (k,v) l r


{-|
   Finds a value in a tree by key. Returns 'Nothing' if the key is not present, otherwise 'Just' value.
   
   Complexity: O(log n) on average.
-}
find :: (Ord k) => k -> Map k v -> Maybe v
find k Empty = Nothing
find k (Node (k1,v1) l r)
 | k < k1 = find k l
 | k > k1 = find k r 
 | otherwise = Just v1 


{-|
   Converts the map to a list of key-value pairs using Inorder Traversal
   
   Complexity: O(n)
-}
map2List :: Map k v -> [(k,v)]
map2List Empty = []
map2List (Node (k,v) esq dir) = map2List esq ++ [(k,v)] ++ map2List dir