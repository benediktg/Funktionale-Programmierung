calcOps :: [(Float -> Float -> Float)] -> Float -> Float -> [Float]
calcOps [] _ _ = []
calcOps (x:xs) a b = (x a b) : (calcOps xs a b)

linearSearch :: (a -> Bool) -> [a] -> Int
linearSearch pre xs = linearSearchRec pre xs 0

linearSearchRec :: (a -> Bool) -> [a] -> Int -> Int
linearSearchRec pre [] n = -1
linearSearchRec pre (x:xs) n = if pre x
                                  then n
                                  else linearSearchRec pre xs (n + 1)
