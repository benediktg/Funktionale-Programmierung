calcOps :: [(Float -> Float -> Float)] -> Float -> Float -> [Float]
calcOps [] _ _ = []
calcOps (x:xs) a b = (x a b) : (calcOps xs a b)
--
linearSearch :: (a -> Bool) -> [a] -> Int
linearSearch pre xs = linearSearchRec pre xs 0

linearSearchRec :: (a -> Bool) -> [a] -> Int -> Int
linearSearchRec pre [] n = -1
linearSearchRec pre (x:xs) n =
    if pre x
       then n
       else linearSearchRec pre xs (n + 1)
--
quickSortGen :: (a -> a -> Bool) -> [a] -> [a]
quickSortGen _ [] = []
quickSortGen cmp (x:xs) =
    let lower = quickSortGen cmp (filter (cmp x) xs)
        upper = quickSortGen cmp (filter (not.cmp x) xs)
    in lower ++ [x] ++ upper
