middleOfThree :: Float -> Float -> Float -> Float
middleOfThree a b c
    | ((b < a) && (a < c)) || ((c < a) && (a < b)) = a
    | ((a < b) && (b < c)) || ((c < b) && (b < a)) = b
    | otherwise = c

howManyEqualOfThree :: Int -> Int -> Int -> Int
howManyEqualOfThree a b c
    | (a == b) && (b == c) = 3
    | (a == b) || (b == c) || (a == c) = 2
    | otherwise = 1

howManyEqualOfFour :: Int -> Int -> Int -> Int
howManyEqualOfFour a b c d
    | (a == d) || (b == d) || (c == d) = 1 + howManyEqualOfThree a b c
    | otherwise = howManyEqualOfThree a b c
    -- problem: e.g. 1 2 2 1 ==> would return 3 but should 2
