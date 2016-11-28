numOddEvenSquare :: Int -> (Int, Int)
numOddEvenSquare n =
    (length oddSq, length evenSq)
    where squares = filter (< n) (map (\ x -> x * x) [1..n])
          oddSq = filter odd squares
          evenSq = filter even squares
--
length' :: [a] -> Int
length' xs = foldl (+) 0 (map (const 1) xs)
--
reverse' :: [a] -> [a]
reverse' xs = foldl rev [] xs
    where rev ys x = x : ys
