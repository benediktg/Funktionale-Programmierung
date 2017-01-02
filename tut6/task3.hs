-- a)
prefixSum :: Num a => [a] -> [a]
prefixSum [] = []
prefixSum (x:xs) = x : [(y + z) | (y, z) <- zip (prefixSum (x : xs)) (xs)]

prefixSum' :: Num a => [a] -> [a]
prefixSum' [] = []
prefixSum' (x:xs) = x : [y + x | y <- prefixSum' xs]

-- b)
primes :: [Int]
primes = sieve [2..]
    where sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]
