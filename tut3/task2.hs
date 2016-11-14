numOfOccur :: [Int] -> Int -> Int
numOfOccur [] _ = 0
numOfOccur (x:xs) a = (if (a == x) then 1 else 0) + numOfOccur xs a

isPalin :: [Int] -> Bool
isPalin xs = xs == reverse xs

primeTwins :: Int -> [(Int, Int)]
primeTwins x = [(a - 2, a) | a <- [3..x], prime a && prime (a - 2)]

-- primeTwins' :: Int -> [(Int, Int)] -> [(Int, Int)]
-- primeTwins' x l
--     | x < 5 = l
--     | prime x && prime (x - 2) = primeTwins' (x - 2) ((x - 2, x):l)
--     | otherwise primeTwins' (x - 1) l

prime :: Int -> Bool
prime n = (divisors n == [1,n])

divisors :: Int -> [Int]
divisors n = [d | d <- [1..n], n `mod` d == 0]

splitList :: [(Int, Int)] -> ([Int], [Int])
splitList xs = ([a | (a, _) <- xs], [b | (_, b) <- xs])

-- splitList' :: [(Int, Int)] -> ([Int], [Int])
-- splitList' xs = (splitListL xs, splitListR xs)
--
-- splitListL :: [(Int, Int)] -> [Int] -> [Int]
-- splitListL ((a, _):xs) =
--
-- splitListR :: [(Int, Int)] -> [Int] -> [Int]
