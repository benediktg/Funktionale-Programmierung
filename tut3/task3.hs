sumNorm :: [Float] -> Float
sumNorm xs
    | null xs = 0.0
    | otherwise = abs (head xs) + sumNorm (tail xs)

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
    let xs1 = take (div (length xs) 2) xs
        xs2 = drop (div (length xs) 2) xs
    in merge (mergeSort xs1) (mergeSort xs2)

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
