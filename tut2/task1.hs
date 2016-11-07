calcFibonacci :: Int -> Int
calcFibonacci n =
    if n <= 2
       then 1
       else calcFibonacci (n - 1) + calcFibonacci (n - 2)

calcPi2 :: Int -> Float
calcPi2 n
    | n == 0 = 1
    | otherwise = (-1)^n / fromIntegral (2 * n + 1) + calcPi2 (n-1)

calcPi :: Float
calcPi = 4 * calcPi2 10000

ggT :: Int -> Int -> Int
ggT a b
    | a > b = ggT (a - b) b
    | a < b = ggT (b - a) a
    | otherwise = a
