module Map (Map, empty, insert, delete, find, map2List, isEmpty) where 

-- TODO add AVL
-- TODO add garbage collection
data Map k v = Empty | Node (k,v) (Map k v) (Map k v) deriving Show

empty :: Map k v
empty = Empty

isEmpty :: Map k v -> Bool 
isEmpty Empty = True 
isEmpty _ = False

-- insert
insert :: (Ord k) => k -> v -> Map k v -> Map k v
insert k v Empty = Node (k,v) Empty Empty
insert k v (Node (k1,v1) l r)
 | k > k1 = Node (k1,v1) l (insert k v r)
 | k < k1 = Node (k1,v1) (insert k v l) r
 | otherwise = Node (k,v) l r

-- find
find :: (Ord k) => k -> Map k v -> Maybe v
find k Empty = Nothing
find k (Node (k1,v1) l r)
 | k < k1 = find k l
 | k > k1 = find k r 
 | otherwise = Just v1 

-- delete
maisEsq :: Map k v -> (k, v) 
maisEsq (Node (k, v) Empty _) = (k, v) 
maisEsq (Node _ esq _) = maisEsq esq

delete :: Ord k => k -> Map k v -> Map k v 
delete k Empty = Empty
delete k (Node (k1, _) Empty dir)
 | k == k1 = dir 
delete k (Node (k1, _) esq Empty)
 | k == k1 = esq 

delete k (Node (k1, v1) esq dir)
 | k < k1 = Node (k1, v1) (delete k esq) dir 
 | k > k1 = Node (k1, v1) esq (delete k dir) 
 | k == k1 = let (zk, zv) = maisEsq dir 
          in Node (zk, zv) esq (delete zk dir) 


map2List :: Map k v -> [(k,v)]
map2List Empty = []
map2List (Node (k,v) esq dir) = map2List esq ++ [(k,v)] ++ map2List dir