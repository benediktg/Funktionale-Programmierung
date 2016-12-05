-- a)
data GenTree a = Leaf a
               | Branch [GenTree a]

-- b)
treeCount :: (a -> Bool) -> GenTree a -> Int
treeCount p (Leaf x) = if p x then 1 else 0
treeCount p (Branch xs) = sum (map (treeCount p) xs)

-- c)
treeList :: GenTree a -> [a]
treeList (Leaf x) = [x]
treeList (Branch []) = []
treeList (Branch xs) = foldl (++) [] (map treeList xs)

-- d)
treeDepth :: GenTree a -> Int
treeDepth (Leaf x) = 1
treeDepth (Branch []) = 0
treeDepth (Branch xs) = foldl (max) 0 (map treeDepth xs) + 1

treeChilds :: GenTree a -> Int
treeChilds (Leaf x) = 0
treeChilds (Branch []) = 0
treeChilds (Branch xs) = foldl (max) 0 (map treeChilds xs)
