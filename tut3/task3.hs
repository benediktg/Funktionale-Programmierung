sumNorm :: [Float] -> Float
sumNorm xs
    | null xs = 0.0
    | otherwise = abs (head xs) + sumNorm (tail xs)

mergeSort :: [Int] -> [Int]
mergeSort xs
    | length xs <= 1 = xs
    | otherwise = let xs1 = take (div (length xs) 2) xs
                      xs2 = drop (div (length xs) 2) xs
                  in reverse (merge (mergeSort xs1) (mergeSort xs2))

merge :: [Int] -> [Int] -> [Int]
merge (x:xs) (y:ys)
    | not (null xs) && not (null ys) = if x <= y
                                          then (x:(merge xs (y:ys)))
                                          else (y:(merge (x:xs) ys))
    | null ys = (x:(merge xs []))
    | null xs = (y:(merge [] ys))
    | otherwise = []
