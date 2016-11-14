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

howManyEqualOfFour :: Int -> Int -> Int -> Int -> Int
howManyEqualOfFour a b c d
    | a > b = howManyEqualOfFour b a c d
    | b > c = howManyEqualOfFour a c b d
    | c > d = howManyEqualOfFour a b d c
    | a < b = howManyEqualOfThree b c d
    | b < c = 2
    | c < d = 3
    | otherwise = 4
