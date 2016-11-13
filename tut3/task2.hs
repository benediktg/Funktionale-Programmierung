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

primeTwins :: Int -> [(Int, Int)]
primeTwins x =
    [(a - 2, a) | a <- [(1 + 2)..x], prime a && prime (a - 2)]

prime :: Int -> Bool
prime n = (divisors n == [1,n])

divisors :: Int -> [Int]
divisors n = [d | d <- [1..n], n `mod` d == 0]

splitList :: [(Int, Int)] -> ([Int], [Int])
splitList xs =
    let ys = [fst x | x <- xs]
        zs = [snd x | x <- xs]
    in (ys, zs)
