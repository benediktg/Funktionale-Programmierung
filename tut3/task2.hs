numOfOccur :: [Int] -> Int -> Int
numOfOccur xs x = numOfOccurRec xs x 0

numOfOccurRec :: [Int] -> Int -> Int -> Int
numOfOccurRec [] x n = n
numOfOccurRec xs x n
    | (head xs) == x = numOfOccurRec (tail xs) x (n + 1)
    | otherwise = numOfOccurRec (tail xs) x n

isPalin :: [Int] -> Bool
isPalin xs = isPalinRec xs (reverse xs)

isPalinRec :: [Int] -> [Int] -> Bool
isPalinRec [] [] = True
isPalinRec xs xsRev
    | head xs == head xsRev = isPalinRec (tail xs) (tail xsRev)
    | otherwise = False
